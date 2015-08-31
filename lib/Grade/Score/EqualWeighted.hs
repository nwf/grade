-- | Offers a section of equally-weighted dings.
--
-- Expects the section header to specify the section maximum
--
-- Dings must have no arguments
module Grade.Score.EqualWeighted (sectyEqualWeighted) where

import           Control.Exception (assert)
import           Data.Monoid (Sum(getSum))
import           Numeric
import           Grade.Types (SecCallback(..))
import qualified Text.Trifecta         as T

efid :: (T.TokenParsing f) => f Double
efid = (either fromIntegral id) <$> T.integerOrDouble

parseDing :: (Applicative f) => f (Sum Int,Sum Int)
parseDing = pure (1, 1)

impact :: Double -> Sum Int -> Sum Int -> Double
impact smax ntotal ndinged = smax / fis ntotal * fis ndinged
 where
  fis = fromIntegral . getSum

printDing :: Double -> Sum Int -> Sum Int -> Maybe String
printDing smax ntotal ding = assert (getSum ding == 1)
                             $ Just (showFFloat (Just 1) (0.0 - impact smax ntotal ding) "") 

scorefn :: Double -> Sum Int -> Sum Int -> Either String Double
scorefn smax ntotal ndinged = Right $ smax - impact smax ntotal ndinged

sectyEqualWeighted :: (T.TokenParsing f) => f (SecCallback f)
sectyEqualWeighted = (\smax -> SC parseDing (printDing smax) (scorefn smax) (\_ -> smax)) <$> efid
