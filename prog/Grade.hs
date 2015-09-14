{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (when)
import           Data.Data (Data)

import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.PrettyPrint.Free        as PP
import qualified Text.Trifecta                as T
import qualified Text.Trifecta.Delta          as T

import qualified System.Console.CmdLib        as C
import           System.Directory
import           System.FilePath
import           System.Exit
import           System.IO

import           Grade.Parse
import           Grade.Grade
import           Grade.Skeleton
import           Grade.Print

import qualified Grade.Score.EqualWeighted  as GSE
import qualified Grade.Score.Simple         as GSS
import qualified Grade.Score.SectionOnly    as GSSO
import qualified Grade.Score.Bounding       as GSB
import qualified Grade.Score.Commenting     as GSC
import qualified Grade.Score.Zeroing        as GSZ
import           Grade.Types

sectys :: T.TokenParsing m => m (ExSecCallback m)
sectys = T.choice
  [ -- A shortcut
    T.symbolic '0'      *> (    GSZ.zeroing zs
                            <$> GSB.bounding GSB.Both
                            <$> GSC.commenting cs
                            <$> GSS.sectySimple)

  , -- Look ma, a little language
    -- Base cases
    T.symbol "simple"   *> GSS.sectySimple
  , T.symbol "equal"    *> GSE.sectyEqualWeighted
  , T.symbol "seconly"  *> GSSO.sectySectionOnly

  , -- Recursive cases
    T.symbol "bounding"   *> (GSB.bounding GSB.Both <$> sectys)
  , T.symbol "nonneg"     *> (GSB.bounding GSB.Below <$> sectys)
  , T.symbol "commenting" *> (GSC.commenting cs <$> sectys)
  , T.symbol "zeroing"    *> (GSZ.zeroing zs <$> sectys)
  ]
 where
  zs = T.symbol "!0" *> pure ()
  cs = T.symbol "!C" *> pure ()

---

doMakeSkeleton :: String -> IO ()
doMakeSkeleton defi = do
  mdefines <- T.parseFromFileEx (parseDefns sectys) defi
  case mdefines of
    T.Failure f -> hPutStrLn stderr (show f)
    T.Success d -> print $ makeSkel d

withDefinesOrElse :: FilePath -> (Defines T.Parser T.Caret -> IO a) -> IO a
withDefinesOrElse defi act = do
  mdefines <- T.parseFromFileEx (parseDefns sectys) defi
  case mdefines of
    T.Failure f -> do
      hPutStrLn stderr "Error while parsing defines:"
      hPutStrLn stderr (show f)
      exitWith (ExitFailure 2)
    T.Success defs -> act defs

grade :: Defines T.Parser T.Caret -> FilePath -> (forall e . PP.Doc e -> IO Bool) -> IO Bool
grade defs dati act = do
  mdata <- T.parseFromFileEx (parseData defs) dati
  case mdata of
    T.Failure f -> do
       hPutStrLn stderr $ "Error while parsing data file " ++ dati ++ ":"
       hPutStrLn stderr (show f)
       pure False
    T.Success (dats, errs) ->
      case errs of
        [] -> case gradeOne defs dats of
                Left e -> do
                  hPutStrLn stderr $ "Error while grading " ++ dati ++ ":"
                  hPutStrLn stderr $ show $ PP.vcat
                                   $ map (printReportError showcaret) e
                  pure False
                Right r -> act (printReport r)
        _  -> do
               hPutStrLn stderr $ "Error while parsing data file " ++ dati ++ ":"
               hPutStrLn stderr $ show $ PP.vcat
                                $ map (printReportError showcaret) errs
               pure False

  where
   showcaret = PP.text . show . TPP.pretty . T.delta

doGradeOne :: FilePath -> FilePath -> IO ()
doGradeOne defi dati = withDefinesOrElse defi $ \defs -> do
  grade defs dati (\d -> PP.hPutDoc stdout d *> pure True) >>= \case
    True  -> pure ()
    False -> exitWith (ExitFailure 1)

doGradeDir :: Int -> String -> FilePath -> FilePath -> IO ()
doGradeDir verbose defi datd outd = withDefinesOrElse defi $ \defs -> do
  createDirectoryIfMissing True outd
  dentries <- getDirectoryContents datd
  oks <- flip mapM dentries $ \dentry -> do
             doesFileExist (datd </> dentry) >>= \ case
               False -> do
                 when (verbose > 1) $ hPutStrLn stderr $ "Skipping " ++ dentry ++ ": is directory"
                 pure True  -- Skip inner directories
               True -> do
                 when (verbose > 0) $ hPutStrLn stderr $ "Grading " ++ dentry
                 grade defs (datd </> dentry)
                       (\d -> withFile (outd </> dentry) WriteMode (flip PP.hPutDoc d) *> pure True)
  exitWith $ if and oks then ExitSuccess else ExitFailure 1

data Cmd = MakeSkeleton { defines :: String }
         | GradeOne { defines :: String, datafile :: String }
         | GradeDir { defines :: String, inDir :: String, outDir :: String, verbose :: Int }
 deriving (Data,Eq,Show)
instance C.Attributes Cmd
instance C.RecordCommand Cmd where
  mode_summary (MakeSkeleton {}) = "Make grader skeleton from defines file"
  mode_summary (GradeOne {})     = "Grade one student file"
  mode_summary (GradeDir {})     = "Grade a directory of student files"

  rec_options (MakeSkeleton {}) = (defines  C.%> C.Default ("/dev/fd/0" :: String)
                                            C.%+ C.Positional 0
                                            C.%+ C.Required True)

  rec_options (GradeOne {})     = (defines  C.%> C.Positional 0
                                            C.%+ C.Required True)
                                  C.%%
                                  (datafile C.%> C.Long ["data"]
                                            C.%+ C.Help "grade data file"
                                            C.%+ C.Default ("/dev/fd/0" :: String))

  rec_options (GradeDir {})     = (defines  C.%> C.Positional 0
                                            C.%+ C.Required True)
                                  C.%%
                                  (outDir   C.%> C.Positional 1
                                            C.%+ C.Required True)
                                  C.%%
                                  (inDir    C.%> C.Positional 2
                                            C.%+ C.Help "defaults to $PWD"
                                            C.%+ C.Default ("." :: FilePath))
                                  C.%%
                                  (verbose  C.%> C.Long ["verbose"]
                                            C.%+ C.ArgHelp "V"
                                            C.%+ C.Help "be chatty (0 to 2)"
                                            C.%+ C.Default (1 :: Int))

  run' (MakeSkeleton {defines}) _ = doMakeSkeleton defines
  run' (GradeOne {defines, datafile}) _ = doGradeOne defines datafile
  run' (GradeDir {defines, inDir, outDir, verbose}) _ = doGradeDir verbose defines inDir outDir

main' :: [String] -> IO ()
main' ars = C.dispatchR [] ars >>= \(x :: Cmd) -> C.run' x []

main :: IO ()
main = C.getArgs >>= main'
