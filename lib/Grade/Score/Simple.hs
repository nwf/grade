-- | A Simple section just adds absolute and relative influences
-- on a score, which is presumed to get full credit if no dings
-- are given.
--
-- Expects the section header to specify the section maximum
--
-- Accepts a number or number-followed-by-%-sign for its ding arg.
module Grade.Score.Simple (sectySimple) where

import           Numeric
import qualified Text.Trifecta         as T
import           Grade.Types (ExSecCallback(..), SecCallback(..))

data Score = S Double Double
 deriving (Show)

instance Monoid Score where
  mempty = S 0.0 0.0
  mappend (S la lr) (S ra rr) = S (la + ra) (lr + rr)

efid :: (T.TokenParsing f) => f Double
efid = (either fromIntegral id) <$> T.integerOrDouble

parseDingScore :: (T.TokenParsing f) => f (Score,())
parseDingScore = (\x -> (x,())) <$> T.choice
  [ -- A number followed by a '%' sign is a relative modifier
    T.try ( ((\n -> S 0.0 (n/100.0)) <$> efid) <* T.symbolic '%' )

    -- A number by itself is an absolute modifier
  , (\n -> S n 0.0) <$> efid
  ]

impact :: Double -> Score -> Double
impact sm (S a r) = a + (sm * r)

printfn :: Double -> () -> () -> Score -> Maybe String
printfn sm () () s = Just $ case s of
                              (S 0.0 0.0) -> "0"
                              (S 0.0 r) -> (p (r*100)) ++ "% == " ++ si
                              (S _ 0.0) -> si
                              (S a r  ) -> (p a) ++ " and " ++ (p r) ++ "% == " ++ si
 where
  si = p $ impact sm s
  p x = showFFloat (Just 1) x ""

scorefn :: Double -> () -> () -> Score -> Either String Double
scorefn sm () () s = Right $ sm + impact sm s

sectySimple_ :: (T.TokenParsing f) => f (SecCallback f () () Score)
sectySimple_ = (\smax -> SC (Nothing, pure ())
                            parseDingScore
                            (printfn smax)
                            (scorefn smax)
                            (\_ -> smax))
               <$> efid

sectySimple :: (T.TokenParsing f) => f (ExSecCallback f)
sectySimple = ExSecCB <$> sectySimple_
