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
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import qualified Data.Char             as C
import qualified Data.Map              as M
import qualified Data.String           as S
import           Data.Maybe (isJust)
import qualified Text.Trifecta         as T
import qualified Text.Trifecta.Delta   as T
import qualified Text.Parser.LookAhead as T

import Grade.Types

------------------------------------------------------------------------ }}}
-- Common -------------------------------------------------------------- {{{

commentStart, commentEnd :: (S.IsString s) => s
commentStart = "$BEGIN_COMMENTS"
commentEnd   = "$END_COMMENTS"

toUtf8 :: (Monad f, T.Parsing f) => f ByteString -> f Text
toUtf8 = (>>= either (\e -> T.unexpected ("Invalid UTF-8: " ++ show e)) (pure) . decodeUtf8')

-- | Grab a comment beginning with # and going to end of line.
hashComment :: T.DeltaParsing f => f Text
hashComment = toUtf8 (T.sliced (T.char '#' *> many (T.noneOf "\r\n"))) <* T.whiteSpace
-- hashComment = T.sliced (T.char '#' *> T.manyTill T.anyChar T.newline) <* T.whiteSpace

-- | Grab a word in its entirety.  Note that this is a little strange as
-- we check the 'notFollowedBy' condition *first*!
word :: (T.DeltaParsing f) => f Text
word = toUtf8 (T.sliced (many $ T.satisfy (not . C.isSpace))) <* T.whiteSpace

------------------------------------------------------------------------ }}}
-- Defines ------------------------------------------------------------- {{{

-- | Grab a ByteString that is a wad of text terminated by a dot on a line
-- by itself.  This terminating line is not included.
untilDotLine :: (T.DeltaParsing f, T.LookAheadParsing f) => f Text
untilDotLine = toUtf8 (T.sliced (T.manyTill T.anyChar (T.try $ T.lookAhead end)))
                <* end <* T.whiteSpace
 where
  end = T.newline *> T.char '.' *> T.newline

-- | Given a parser for X, parse lines of the form ":-name X" preceeded by
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
                 => f (SecCallback f) -> f (SecName, T.Caret, ExSection T.Caret)
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
    SC fsdt sdpo sfn smaxfn -> do
      (sstate, sdings) <- getDings fsdt M.empty
      return (sname, c, ExSec $
               Sec (SecMeta stitle (smaxfn sstate) (sfn sstate) (sdpo sstate)) shidden scs sdings)
 where
  getDings sdp = go mempty
   where
    go s m = nextDing s m <|> return (s,m)

    nextDing s m = do
     (dn, ds, db) <- parseDingDefn sdp
     case M.lookup dn m of
       Nothing -> go (s `mappend` ds) (M.insert dn db m)
       Just db' -> do
         T.raiseErr (T.Err (Just "Duplicate ding definition") [] mempty)

-- | Parse a definitions file
parseDefns :: (T.DeltaParsing f, T.MarkParsing T.Delta f, T.Errable f, T.LookAheadParsing f)
           => f (SecCallback f) -> f (Defines T.Caret)
parseDefns sectys = T.whiteSpace *> (Defs <$> go M.empty) <* T.eof
  where
   go m = nextSection m <|> return m
   nextSection m = do
     (sn, sc, sb) <- parseSectionDefn sectys
     case M.lookup sn m of
       Nothing -> go (M.insert sn (sb, sc) m)
       Just (_, dc') -> do
         T.release (T.delta sc)
         T.raiseErr (T.Err (Just "Duplicate section definition") [] mempty)

------------------------------------------------------------------------ }}}
-- Data ---------------------------------------------------------------- {{{

-- | Parse a grader data file
parseData :: (T.DeltaParsing f, T.LookAheadParsing f) => f (DataFile T.Caret)
parseData = T.whiteSpace *> (DF <$> many dataSection) <* T.eof
 where
  dataSection = do
    _   <- many hashComment
    _ T.:^ sc <- T.careted $ T.char '@'
    sn  <- SN <$> word
    ds  <- many (T.try (many hashComment *> T.symbolic ':')
                 *> fmap (\(d T.:^ c) -> (d,c)) (T.careted (DN <$> word)))
    _   <- many hashComment
    mcs <- T.optional $ 
              T.string commentStart *> T.newline *>
              toUtf8 (T.sliced (T.manyTill T.anyChar (T.lookAhead cend))) <* cend
    _   <- T.whiteSpace
    return $ DFS sn sc ds mcs

  cend = T.string commentEnd *> T.newline

------------------------------------------------------------------------ }}}
