-- | Allow the definition of dings that do not affect the score
--   in any way.
--
--   When it comes time to print, there won't be any (0.0) or
--   similar at the top of the comment block.

module Grade.Score.Commenting (commenting) where

import           Control.Monad (join)
import qualified Text.Trifecta         as T
import           Grade.Types (ExSecCallback(..), SecCallback(..))

parseCommented :: (T.TokenParsing f, Monoid sds)
               => f ()        -- ^ How do we parse a comment-only ding?
               -> f (sdt,sds) -- ^ What is the underlying ding parser?
               -> f (Maybe sdt, sds)
parseCommented pc pd = T.choice
  [ -- Try parsing a zeroizing form
    T.try pc *> pure (Nothing, mempty)
  , -- Otherwise, invoke the underlying parser
    (\(a,b) -> (Just a, b)) <$> pd
  ]

printCommented :: (sds -> sat -> sdt -> Maybe String)
               -> sds -> sat -> Maybe sdt -> Maybe String
printCommented po ss sa = join . fmap (po ss sa)

scoreCommented :: Monoid sdt
               => (sds -> sat -> sdt -> Either String Double)
               -> sds -> sat -> Maybe sdt -> Either String Double
scoreCommented ug ss sa = ug ss sa . maybe mempty id

commenting :: (T.TokenParsing f) => f () -> ExSecCallback f -> ExSecCallback f
commenting pc shp =
  case shp of
    ExSecCB (SC us up uo ug um) ->
      ExSecCB (SC us
                  (parseCommented pc up)
                  (printCommented uo)
                  (scoreCommented ug)
                  um)
