{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Grade.Grade (gradeOne) where

import           Control.Lens
import           Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Grade.Types

collectErrors :: [Either e a] -> Either [e] [a]
collectErrors x = case partitionEithers x of
                    ([], r) -> Right r
                    (l, _)  -> Left l

processDFS :: ExDFS loc
           -> Either String ReportFileSection
processDFS (ExDFS (DFS (SecMeta stitle smax sfn sdpo) _ dds dgcs)) = do
  sscore <- sfn $ mconcat $ (_dm_mod . _dfd_meta) <$> dds
  pure $ RFS stitle sscore smax (dopo <$> dds) dgcs
 where
  dopo d = T.unlines $ addMod $ pure $ (_dm_text . _dfd_meta) d
   where
    addMod = if smax == 0.0
              then id -- Don't print if section is worthless
              else maybe id -- Don't print if the section chooses to not
                         ((:) . T.cons '(' . flip T.snoc ')' . T.pack) -- Add parens otherwise
                         (sdpo (_dm_mod $ _dfd_meta d))

gradeOne :: Defines loc' -> DataFile loc -> Either [ReportError loc] ReportFile
gradeOne (Defs defs _) (DF dfss) =
  let availScores  = collectErrors $ fmap processSec dfss in
  let residualSecs = M.keysSet $ foldr (\(k,_) f -> (M.delete k) . f) id dfss defs in
  if S.null residualSecs
   then RF <$> availScores
   else let e = REMissingSections residualSecs in
        case availScores of
          Left es -> Left (e:es)
          Right _ -> Left [e]
 where
  processSec (sn,s) = bimap (RESectionError sn . pure . SEScoreError) id $ processDFS s
