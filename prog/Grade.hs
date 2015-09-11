{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Data (Data)

import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.PrettyPrint.Free        as PP
import qualified Text.Trifecta                as T
import qualified Text.Trifecta.Delta          as T

import qualified System.Console.CmdLib        as C
import           System.IO

import           Grade.Parse
import           Grade.Grade
import           Grade.Skeleton
import           Grade.Print

import qualified Grade.Score.EqualWeighted  as GSE
import qualified Grade.Score.Simple         as GSS
import qualified Grade.Score.Bounding       as GSB
import qualified Grade.Score.Zeroing        as GSZ
import           Grade.Types

sectys :: T.TokenParsing m => m (ExSecCallback m)
sectys = T.choice
  [ -- A shortcut
    T.symbolic '0'      *> (GSZ.zeroing zs <$> GSB.bounding GSB.Both <$> GSS.sectySimple)

  , -- Look ma, a little language
    -- Base cases
    T.symbol "simple"   *> GSS.sectySimple
  , T.symbol "equal"    *> GSE.sectyEqualWeighted

  , -- Recursive cases
    T.symbol "bounding" *> (GSB.bounding GSB.Both <$> sectys)
  , T.symbol "nonneg"   *> (GSB.bounding GSB.Below <$> sectys)
  , T.symbol "zeroing"  *> (GSZ.zeroing zs <$> sectys)
  ]
 where
  zs = T.symbol "!0" *> pure ()

---

doMakeSkeleton :: String -> IO ()
doMakeSkeleton defi = do
  mdefines <- T.parseFromFileEx (parseDefns sectys) defi
  case mdefines of
    T.Failure f -> hPutStrLn stderr (show f)
    T.Success d -> print $ makeSkel d

doGradeOne :: String -> String -> IO ()
doGradeOne defi dati = do
  mdefines <- T.parseFromFileEx (parseDefns sectys) defi
  case mdefines of
    T.Failure f -> hPutStrLn stderr "Error while parsing defines:"
                   *> hPutStrLn stderr (show f)
    T.Success defs -> do
      mdata <- T.parseFromFileEx (parseData defs) dati
      case mdata of
        T.Failure f -> hPutStrLn stderr "Error while parsing data:"
                       *> hPutStrLn stderr (show f)
        T.Success (dats, errs) -> case errs of
                                    [] -> case gradeOne defs dats of
                                            Left e -> do
                                                       hPutStrLn stderr "Error while grading:"
                                                       hPutStrLn stderr $ show $ PP.vcat
                                                                        $ map (printReportError showcaret) e
                                            Right r -> print $ printReport r
                                    _  -> do
                                           hPutStrLn stderr "Error while parsing data:"
                                           hPutStrLn stderr $ show $ PP.vcat
                                                            $ map (printReportError showcaret) errs

  where
   showcaret = PP.text . show . TPP.pretty . T.delta


data Cmd = MakeSkeleton { defines :: String }
         | GradeOne { defines :: String, datafile :: String }
         -- XXX TODO | GradeDir { defines :: String, in_dir :: String, out_dir :: String }
 deriving (Data,Eq,Show)
instance C.Attributes Cmd
instance C.RecordCommand Cmd where
  mode_summary (MakeSkeleton {}) = "Make grader skeleton from defines file"
  mode_summary (GradeOne {})     = "Grade one student file"

  rec_options (MakeSkeleton {}) = defines C.%> C.Default ("/dev/fd/0" :: String)
                                          C.%+ C.Positional 0
                                          C.%+ C.Required True
  rec_options (GradeOne {})     = (defines C.%> C.Positional 0
                                           C.%+ C.Required True)
                                  C.%%
                                  (datafile C.%> C.Long ["data"]
                                            C.%+ C.Help "grade data file"
                                            C.%+ C.Default ("/dev/fd/0" :: String))

  run' (MakeSkeleton {defines = i}) _ = doMakeSkeleton i
  run' (GradeOne { defines = defi, datafile = dati }) _ = doGradeOne defi dati

main' :: [String] -> IO ()
main' ars = C.dispatchR [] ars >>= \(x :: Cmd) -> C.run' x []

main :: IO ()
main = C.getArgs >>= main'
