-- | Allow the defines file to indicate dings that will zero out a section.
module Grade.Score.Zeroing (zeroing) where

import qualified Text.Trifecta         as T
import           Grade.Types (SecCallback(..))

data Zeroing a = Zeroed | Earned a
 deriving (Show)

instance Monoid a => Monoid (Zeroing a) where
  mempty = Earned mempty

  mappend Zeroed     _          = Zeroed
  mappend _          Zeroed     = Zeroed
  mappend (Earned l) (Earned r) = Earned (l `mappend` r)

parseZeroed :: (T.TokenParsing f, Monoid sds)
            => f ()        -- ^ How do we parse a zeroizing ding?
            -> f (sdt,sds) -- ^ What is the underlying ding parser?
            -> f (Zeroing sdt, sds)
parseZeroed pz pd = T.choice
  [ -- Try parsing a zeroizing form
    T.try pz *> pure (Zeroed, mempty)
  , -- Otherwise, invoke the underlying parser
    (\(a,b) -> (Earned a, b)) <$> pd
  ]

printZeroed :: (sds -> sdt -> Maybe String)
            -> sds -> Zeroing sdt -> Maybe String
printZeroed po ss r = case r of
                        Zeroed   -> Just "Score set to 0"
                        Earned v -> po ss v

scoreZeroed :: (sds -> sdt -> Either String Double)
            -> sds -> Zeroing sdt -> Either String Double
scoreZeroed ug ss r = case r of
                         Zeroed    -> Right 0.0
                         Earned r' -> ug ss r'

zeroing :: (T.TokenParsing f) => f () -> SecCallback f -> SecCallback f
zeroing pz shp = 
  case shp of SC up uo ug um -> SC (parseZeroed pz up) (printZeroed uo) (scoreZeroed ug) um
