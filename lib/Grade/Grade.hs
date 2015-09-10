{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Grade.Grade (gradeOne) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Either
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           Grade.Types

collectErrors :: [Either e a] -> Either [e] [a]
collectErrors x = case partitionEithers x of
                    ([], r) -> Right r
                    (l, _)  -> Left l

lookupSectionDings :: [(DingName, loc)]
                   -> M.Map DingName (Ding sdt loc')
                   -> Either [SectionError loc] [(Ding sdt loc', loc)]
lookupSectionDings dns0 sm = collectErrors $ flip evalState M.empty $ mapM look dns0
 where
  look (d,loc) = do
    already <- gets (M.lookup d)
    case already of
      Just loc' -> return $ Left $ SEDuplicateDing d loc' loc
      Nothing -> case M.lookup d sm of
                   Nothing -> return $ Left $ SEUndefinedDing d loc
                   Just dd -> do
                               when (not $ _ding_multiple dd) $ modify (M.insert d loc)
                               return $ Right (dd,loc)

dingsToScore :: ExSection loc'
             -> [(DingName, loc)]
             -> Either [SectionError loc] (T.Text, Double, Double, [T.Text])
dingsToScore es dns =
  case es of
    ExSec (Sec (SecMeta stitle smax sfn spo) _ _ sdm) ->
      either Left (either (Left . pure . SEScoreError)
                          (\(sc, ds) -> Right (stitle, sc, smax, map dopo ds)))
      $ bimap id (reduce . map fst) $ lookupSectionDings dns sdm
      where
       reduce ds = (\x -> (x,ds)) <$> (sfn $ mconcat $ map _ding_mod ds)

       dopo d = T.unlines $ addMod $ pure $ _ding_text d
        where
         addMod = if smax == 0.0
                   then id -- Don't print if section is worthless
                   else maybe id -- Don't print if the section chooses to not
                              ((:) . T.cons '(' . flip T.snoc ')' . T.pack) -- Add parens otherwise
                              (spo (_ding_mod d))

processDFS :: Defines loc'
           -> DataFileSection loc
           -> Either (ReportError loc) (T.Text, Double, Double, [T.Text])
processDFS (Defs sm) (DFS sn sl dns _) = maybe (Left $ REUnknownSection sn sl)
                                               (bimap (RESectionError sn) id)
                                         $ (\(es,_) -> dingsToScore es dns) <$> M.lookup sn sm

checkProcessedAll :: Defines defloc
                  -> (Either [ReportError loc] a, M.Map SecName b)
                  -> Either [ReportError loc] a
checkProcessedAll (Defs sm) (ea, m) =
  let missing = M.keysSet sm `S.difference` M.keysSet m in
  if S.null missing
   then ea
   else Left $ case ea of
                 Right _ -> [REMissingSections missing]
                 Left es -> REMissingSections missing : es

gradeOne :: Defines loc' -> DataFile loc -> Either [ReportError loc] ReportFile
gradeOne defs (DF dfss) = checkProcessedAll defs
                          $ over _1 (fmap RF . collectErrors)
                          $ flip runState M.empty $ mapM go dfss
 where
  go dfs@(DFS sn sloc _ gcs) = do
    already <- gets (M.lookup sn)
    case already of
      Just loc' -> return $ Left $ REDuplicateSection sn loc' sloc
      Nothing -> do
                  modify (M.insert sn sloc)
                  return $ (\(st,sc,smax,bs) -> RFS st sc smax bs gcs) <$> processDFS defs dfs
