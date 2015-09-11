-- | Allow the defines file to indicate dings that will zero out a section.
module Grade.Score.Zeroing (zeroing) where

import qualified Text.Trifecta         as T
import           Grade.Types (ExSecCallback(..), SecCallback(..))

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

printZeroed :: (sds -> sat -> sdt -> Maybe String)
            -> sds -> sat -> Zeroing sdt -> Maybe String
printZeroed po ss sa r = case r of
                          Zeroed   -> Just "Score set to 0"
                          Earned v -> po ss sa v

scoreZeroed :: (sds -> sat -> sdt -> Either String Double)
            -> sds -> sat -> Zeroing sdt -> Either String Double
scoreZeroed ug ss sa r = case r of
                           Zeroed    -> Right 0.0
                           Earned r' -> ug ss sa r'

zeroing :: (T.TokenParsing f) => f () -> ExSecCallback f -> ExSecCallback f
zeroing pz shp = 
  case shp of
    ExSecCB (SC us up uo ug um) ->
      ExSecCB (SC us
                  (parseZeroed pz up)
                  (printZeroed uo)
                  (scoreZeroed ug)
                  um)
