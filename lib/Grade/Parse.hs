-- Header -------------------------------------------------------------- {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Grade.Parse (
  SecCallback(..), parseDefns, parseData, commentStart, commentEnd
) where

import           Control.Applicative
-- import qualified Control.Lens          as L
-- import           Control.Monad (guard, when)
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Text (Text,unpack)
import           Data.Text.Encoding (decodeUtf8')
-- import qualified Data.Char             as C
import qualified Data.Map              as M
-- import qualified Data.Set              as S
import           Data.String (IsString)
-- import           Data.Semigroup ((<>))
import           Data.Maybe (isJust)
import qualified Text.Trifecta         as T
import qualified Text.Trifecta.Delta   as T
import qualified Text.Parser.LookAhead as T
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Grade.Types

------------------------------------------------------------------------ }}}
-- Common -------------------------------------------------------------- {{{

commentStart, commentEnd :: (IsString s) => s
commentStart = "$BEGIN_COMMENTS"
commentEnd   = "$END_COMMENTS"

toUtf8 :: (Monad f, T.Parsing f) => f ByteString -> f Text
toUtf8 = (>>= either (\e -> T.unexpected ("Invalid UTF-8: " ++ show e)) (pure) . decodeUtf8')

-- | Grab a comment beginning with # and going to end of line.
hashComment :: T.DeltaParsing f => f Text
hashComment = toUtf8 (T.sliced (T.char '#' *> many (T.noneOf "\r\n"))) <* T.whiteSpace
-- hashComment = T.sliced (T.char '#' *> T.manyTill T.anyChar T.newline) <* T.whiteSpace

-- | Sometimes we want to be more forceful than T.whiteSpace and actually
-- ensure that there is some space or that we're at the end of input.
sseof :: (T.TokenParsing f) => f ()
sseof = (T.someSpace <|> T.eof)

-- | Grab a word in its entirety.  Note that this is a little strange as
-- we check the 'notFollowedBy' condition *first*!
word :: (T.DeltaParsing f) => f Text
word = toUtf8 (T.sliced (many $ T.notFollowedBy T.someSpace *> T.anyChar)) <* sseof

-- | Choose by key in a map
parseMapKeys :: (T.TokenParsing f)
             => (k -> String)
             -> M.Map k v
             -> f (k, v)
parseMapKeys ks m = T.choice $ (uncurry arm) <$> M.toList m
 where
  arm k v = ((T.try ((T.string $ ks k) <* sseof)) *> pure (k,v))
            T.<?> show (ks k)

------------------------------------------------------------------------ }}}
-- Defines ------------------------------------------------------------- {{{

-- | Grab a ByteString that is a wad of text terminated by a dot on a line
-- by itself.  This terminating line is not included.
untilDotLine :: (T.DeltaParsing f, T.LookAheadParsing f) => f Text
untilDotLine = toUtf8 (T.sliced (T.manyTill T.anyChar (T.try $ T.lookAhead end)))
                <* end <* T.whiteSpace
 where
  end = T.newline *> T.char '.' *> T.newline

-- | Given a parser for X, parse lines of the form ":name X" preceeded by
-- any number of "# comment" lines and followed by the ding text, terminated
-- by a dot line.
parseDingDefn :: (T.DeltaParsing f, T.LookAheadParsing f)
              => f (sdt,sds) -> f (DingName, sds, DingDefn sdt T.Caret)
parseDingDefn dl = do
  (dcs, reuse) <- T.try ((,) <$> many (hashComment) <*> leadchar)
  dn T.:^ c <- T.careted (DN <$> word)
  (dm, ds) <- dl
  dt <- untilDotLine
  pure (dn, ds, DingDefn (DingMeta dm dt) c reuse dcs)
 where
  leadchar = T.choice [ T.char ':' *> pure False
                      , T.char ';' *> pure True
                      ]


parseSectionDefn :: (T.DeltaParsing f, T.MarkParsing T.Delta f, T.Errable f, T.LookAheadParsing f)
                 => f (ExSecCallback f) -> f (SecName, ExSection f T.Caret)
parseSectionDefn fsdap = do
  scs <- many hashComment
  _ T.:^ c <- T.careted (T.symbolic '@')
  sname   <- SN <$> word
  shidden <- isJust <$> T.optional (T.char '!')
  esdp <- fsdap
  _       <- T.symbolic '-'
  stitle  <- toUtf8 (T.sliced (T.manyTill T.anyChar (T.lookAhead T.newline)))
  _       <- T.newline
  case esdp of
    ExSecCB (SC mfss fsdt sdpo sfn smaxfn) -> do
      (sstate, sdings) <- getDings fsdt M.empty
      _ <- T.whiteSpace
      return (sname, ExSec $
               Sec (SecMeta stitle (smaxfn sstate) (sfn sstate) (sdpo sstate)) c shidden scs sdings mfss)
 where
  getDings fsdt = go mempty
   where
    go s m = nextDing s m <|> return (s,m)

    nextDing s m = do
     (dn, ds, db) <- parseDingDefn fsdt
     case M.lookup dn m of
       Nothing -> go (s `mappend` ds) (M.insert dn db m)
       Just _ -> do
         T.raiseErr (T.Err (Just "Duplicate ding definition") [] mempty)

-- | Parse a definitions file
parseDefns :: (T.DeltaParsing f, T.MarkParsing T.Delta f, T.Errable f, T.LookAheadParsing f)
           => f (ExSecCallback f) -> f (Defines f T.Caret)
parseDefns sectys = T.whiteSpace *> go M.empty [] <* T.eof
  where
   go m l = nextSection m l <|> return (Defs m (reverse l))
   nextSection m l = do
     (sn, sb) <- parseSectionDefn sectys
     case M.lookup sn m of
       Nothing -> go (M.insert sn sb m) ((sn,sb):l)
       Just _  -> do
         T.release (T.delta $ case sb of ExSec s -> _sec_loc s)
         T.raiseErr (T.Err (Just "Duplicate section definition") [] mempty)

------------------------------------------------------------------------ }}}
-- Data ---------------------------------------------------------------- {{{

-- | Parse a grader data file
parseData :: forall f loc . (T.DeltaParsing f, T.LookAheadParsing f)
          => Defines f loc -> f (DataFile T.Caret, [ReportError T.Caret])
parseData defs = do
  _         <- T.whiteSpace
  (ss, fsm) <- sections defs
  _         <- many hashComment
  _         <- T.eof
  pure (DF ss, (if M.null fsm then id else (REMissingSections (M.keysSet fsm) :)) [])
 where
  sections (Defs sm0 _) = flip runStateT sm0 $ go M.empty
   where
    go already = do
      _ <- many hashComment
      another already <|> pure []

    another already = do
      (sn,esb) T.:^ sc <- get >>= \sm -> sectionDirective sm
      case esb of
        ExSec (Sec smeta _ _ _ sdm (_,fsat)) -> do
          sat <- lift fsat
          ds  <- sectionDings sdm
          mcs <- T.optional $
                    T.string commentStart *> T.newline *>
                    toUtf8 (T.sliced (T.manyTill T.anyChar (T.lookAhead cend))) <* cend
          _   <- T.whiteSpace
          ((sn, ExDFS $ DFS smeta sat sc ds mcs) :)
            <$> (modify (M.delete sn) >> go (M.insert sn () already))

    cend = T.string commentEnd *> T.newline

  sectionDirective = directiveChoice '@' (unpack . unSN)

  sectionDings dm0 = go dm0 M.empty
   where
    go dm already = do
      _ <- many hashComment
      another dm already <|> pure []

    another dm already = do
      ((dn,DingDefn dmeta _ dingmany _) T.:^ dc) <- dingDirective dm
      ((DFD dmeta dc) :) <$> go (if dingmany then dm else M.delete dn dm)
                                (if dingmany then already else M.insert dn () already)

    dingDirective = directiveChoice ':' (unpack . unDN)

  directiveChoice lc f m = do
    _ T.:^ sc <- T.lookAhead (T.careted $ T.char lc)
    (T.:^ sc) <$> parseMapKeys ((lc :) . f) m

{-
-- | Gobble characters until we're looking at something we probably know and
--   love; it's a guess, of course.
recover = T.skipSome (T.notFollowedBy (T.choice (T.try <$> sigil)) *> T.anyChar)
        *> T.whiteSpace
 where
  sigil = [ T.newline *> T.whiteSpace *> T.char '@' *> pure ()
          , T.newline *> T.whiteSpace *> T.char ':' *> pure ()
          , T.char '#' *> pure ()
          , T.symbol commentStart *> pure ()
          ]

dcErr lc fk falr fnew malr myet = do
  _ T.:^ sc <- T.lookAhead (T.careted $ T.char lc)
  T.choice [ (Right . (T.:^ sc))    <$> parseMapKeys ((lc :) . fk) myet
           , (Left . falr sc . fst) <$> parseMapKeys ((lc :) . fk) malr
           , (Left . fnew sc)       <$> (T.char lc *> word <* sseof)
           ]

dcErr' lc fk malr myet = dcErr lc fk (flip SEDuplicateDing) (flip SEUndefinedDing)
-}

------------------------------------------------------------------------ }}}
