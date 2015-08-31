-- | Like Zeroing, this allows overrides of some other section type's score.
-- This one, more generally, allows the defines file to specify overrides
-- other than zero.
module Grade.Score.Setting (sectySetting) where

import           Numeric
import qualified Text.Trifecta         as T
import           Grade.Types (SecCallback(..))

data Setting a = SetTwice | Set Double | Earned a
 deriving (Show)

instance Monoid a => Monoid (Setting a) where
  mempty = Earned mempty

  mappend SetTwice   _          = SetTwice
  mappend _          SetTwice   = SetTwice

  mappend (Set _)    (Set _)    = SetTwice
  mappend s@(Set _)  (Earned _) = s
  mappend (Earned _) s@(Set _)  = s

  mappend (Earned l) (Earned r) = Earned (l `mappend` r)

parseSet :: (T.TokenParsing f, Monoid sds)
         => f Double    -- ^ Parse a score-setting ding
         -> f (sdt,sds) -- ^ What is the underlying ding parser?
         -> f (Setting sdt, sds)
parseSet ps pd = T.choice
  [ -- Try parsing a setting form
    (\s -> (Set s, mempty)) <$> T.try ps
  , -- Otherwise, invoke the underlying parser
    (\(a,b) -> (Earned a, b)) <$> pd
  ]

printSet :: (sds -> sdt -> Maybe String)
         -> sds -> Setting sdt -> Maybe String
printSet po ss r = case r of
                     SetTwice -> error "Score.Setting was asked to print out impossible state"
                     Set v    -> Just $ showFFloat (Just 1) v "Score set to "
                     Earned v -> po ss v

scoreSet :: (sds -> sdt -> Either String Double)
         -> sds -> Setting sdt -> Either String Double
scoreSet ug ss r = case r of
                     SetTwice  -> Left "Multiple score-setting dings in section"
                     Set v     -> Right v
                     Earned r' -> ug ss r'

sectySetting :: (T.TokenParsing f) => f Double -> f (SecCallback f) -> f (SecCallback f)
sectySetting ps = fmap
  (\shp -> case shp of SC up uo ug um -> SC (parseSet ps up) (printSet uo) (scoreSet ug) um)
