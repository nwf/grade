-- | Offers a section of equally-weighted dings, with
-- the section maximum being determined by the number of dings.
--
-- There should be no section heading parameters, and
-- dings must have no arguments.
module Grade.Score.EqualWeightedCounting (sectyEqualWeighted) where

import           Control.Exception (assert)
import           Data.Monoid (Sum(getSum))
import           Numeric
import           Grade.Types (SecCallback(..))
import qualified Text.Trifecta         as T

parseDing :: (Applicative f) => f (Sum Int,Sum Int)
parseDing = pure (1, 1)

fis :: Sum Int -> Double
fis = fromIntegral . getSum

printDing :: Sum Int -> Sum Int -> Maybe String
printDing _ ding = assert (getSum ding == 1) $ Just "-1"

scorefn :: Sum Int -> Sum Int -> Either String Double
scorefn ntotal ndinged = Right $ fis $ ntotal - ndinged

sectyEqualWeighted :: (T.TokenParsing f) => f (SecCallback f)
sectyEqualWeighted = pure $ SC parseDing printDing scorefn (fromIntegral . getSum)
