module Grade.Score.SectionOnly (sectySectionOnly) where

import qualified Text.Trifecta         as T
import           Grade.Types (ExSecCallback(..), SecCallback(..))

efid :: (T.TokenParsing f) => f Double
efid = (either fromIntegral id) <$> T.integerOrDouble

parseDataScore :: (T.TokenParsing f) => f (Maybe Double)
parseDataScore = T.choice [ Just <$> efid, T.symbolic '!' *> pure Nothing ]

sectySectionOnly_ :: (T.TokenParsing f) => f (SecCallback f () (Maybe Double) ())
sectySectionOnly_ = (\smax -> SC (Just $ "<score out of " ++ show smax ++ ">", parseDataScore)
                                 (T.unexpected "Section-only sections do not define dings")
                                 (\_ _ _ -> Nothing)
                                 (\_ d _ -> maybe (Left "No score given for a SectionOnly section!") Right d)
                                 (\_     -> smax)
                    ) <$> efid

sectySectionOnly :: (T.TokenParsing f) => f (ExSecCallback f)
sectySectionOnly = ExSecCB <$> sectySectionOnly_
