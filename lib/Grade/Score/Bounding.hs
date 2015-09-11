-- | Bound scores both above (by section max) and below (by zero)
module Grade.Score.Bounding (BoundHow(..), bounding) where
import           Grade.Types (ExSecCallback(..), SecCallback(..))

data BoundHow = Below | Above | Both

bound :: BoundHow -> Double -> Double -> Double
bound Below _ = max 0.0
bound Above m = min m
bound Both  m = max 0.0 . min m

bounding :: BoundHow -> ExSecCallback f -> ExSecCallback f
bounding how s = 
  case s of
    ExSecCB (SC ps pa po g pm) ->
      ExSecCB (SC ps pa po (\sps sat sdt -> bound how (pm sps) <$> g sps sat sdt) pm)
