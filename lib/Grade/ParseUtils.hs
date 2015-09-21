-- Header -------------------------------------------------------------- {{{

module Grade.ParseUtils (
  toUtf8, sseof, word, hashComment, parseMapKeys
) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Map              as M
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import qualified Text.Trifecta                as T

------------------------------------------------------------------------ }}}

toUtf8 :: (Monad f, T.Parsing f) => f ByteString -> f Text
toUtf8 = (>>= either (\e -> T.unexpected ("Invalid UTF-8: " ++ show e)) (pure) . decodeUtf8')

-- | Sometimes we want to be more forceful than T.whiteSpace and actually
-- ensure that there is some space or that we're at the end of input.
sseof :: (T.TokenParsing f) => f ()
sseof = (T.someSpace <|> T.eof)

-- | Grab a word in its entirety.  Note that this is a little strange as
-- we check the 'notFollowedBy' condition *first*!
word :: (T.DeltaParsing f) => f Text
word = toUtf8 (T.sliced (many $ T.notFollowedBy T.someSpace *> T.anyChar)) <* sseof

-- | Grab a comment beginning with # and going to end of line.
hashComment :: T.DeltaParsing f => f Text
hashComment = toUtf8 (T.sliced (T.char '#' *> many (T.noneOf "\r\n"))) <* T.whiteSpace
-- hashComment = T.sliced (T.char '#' *> T.manyTill T.anyChar T.newline) <* T.whiteSpace

-- | Choose by key in a map
parseMapKeys :: (T.TokenParsing f)
             => (k -> String)
             -> M.Map k v
             -> f (k, v)
parseMapKeys ks m = T.choice $ (uncurry arm) <$> M.toList m
 where
  arm k v = ((T.try ((T.string $ ks k) <* sseof)) *> pure (k,v))
            T.<?> show (ks k)


