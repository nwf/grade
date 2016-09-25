{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Grade.GradeIO (
  withDefines, withDefinesOrElse,
  withReport, withReportDoc
) where

import           System.Exit (ExitCode(..), exitWith)
import           System.IO (hPutStrLn, stderr)
import qualified Text.PrettyPrint.ANSI.Leijen as TPP
import qualified Text.PrettyPrint.Free        as PP
import qualified Text.Trifecta                as T
import qualified Text.Trifecta.Delta          as T

import           Grade.Types
import           Grade.Parse
import           Grade.Print
import           Grade.Grade


withDefines :: T.Parser (ExSecCallback T.Parser)
            -> FilePath
            -> (Defines T.Parser T.Caret -> IO a)
            -> IO (Either TPP.Doc a)
withDefines sectys defi act = do
  mdefines <- T.parseFromFileEx (parseDefns sectys) defi
  case mdefines of
    T.Failure f -> parseErr (T._errDoc f)
    T.Success defs -> Right <$> act defs
 where
  parseErr f = pure $ Left (        "Error while parsing defines" TPP.<+> TPP.pretty defi TPP.<> ":"
                            TPP.</> f)


withDefinesOrElse :: T.Parser (ExSecCallback T.Parser)
                  -> FilePath
                  -> (Defines T.Parser T.Caret -> IO a)
                  -> IO a
withDefinesOrElse sectys defi act = withDefines sectys defi act >>= eh
 where
  eh (Left f) = do
      hPutStrLn stderr (show f)
      exitWith (ExitFailure 2)
  eh (Right a) = pure a

withReport :: Defines T.Parser T.Caret
           -> FilePath
           -> (ReportFile -> IO r)
           -> IO (Either (PP.Doc e) r)
withReport defs dati act = do
  mdata <- T.parseFromFileEx (parseData defs) dati
  case mdata of
    T.Failure e -> parseErr (PP.pretty $ show e) -- XXX; sadness and woe
    T.Success (dats, errs) ->
      case errs of
        [] -> case gradeOne defs dats of
                Left e -> gradeErr (vmpesc e)
                Right r -> Right <$> act r
        _  -> parseErr (vmpesc errs)

  where
   parseErr e = pure $ Left ("Error while parsing data file" PP.<+> PP.pretty dati PP.<> ":" PP.</> e)
   gradeErr e = pure $ Left ("Error while grading" PP.<+> PP.pretty dati PP.<> ":" PP.</> e)
   vmpesc e = PP.vcat (map (printReportError showcaret) e)
   showcaret = PP.text . show . TPP.pretty . T.delta

withReportDoc :: Defines T.Parser T.Caret
              -> FilePath
              -> (forall e . PP.Doc e -> IO ())
              -> IO Bool
withReportDoc defs dati act = withReport defs dati (act . printReport) >>= eh
 where
  eh (Left d) = PP.hPutDoc stderr d >> pure False
  eh (Right ()) = pure True


