-- | Bound scores both above (by section max) and below (by zero)
module Grade.Score.Bounding (BoundHow(..), bounding) where
import           Grade.Types (SecCallback(..))

data BoundHow = Below | Above | Both

bound Below _ = max 0.0
bound Above m = min m
bound Both  m = max 0.0 . min m

bounding :: BoundHow -> SecCallback f -> SecCallback f
bounding how s = 
  case s of SC pa po g pm -> SC pa po (\sps sdt -> bound how (pm sps) <$> g sps sdt) pm
