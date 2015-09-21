{-# LANGUAGE OverloadedStrings #-}

module Grade.Print where

import qualified Data.Char                    as C
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import           Numeric
import           Text.PrettyPrint.Free

import           Grade.Types

printSectionError :: (loc -> Doc e) -> SectionError loc -> Doc e
printSectionError pl (SEUndefinedDing dn dl) =
  "Unknown ding" <+> pretty (unDN dn) <+> "at" <+> pl dl
printSectionError pl (SEDuplicateDing dn dl1 dl2) =
  "Multiple occurrence of single-use ding" <+> pretty (unDN dn) <+> "at:"
  `above` indent 1 (vcat $ map pl [dl1, dl2])
printSectionError _  (SEScoreError se) =
  "Score function error:" <+> pretty se

printReportError :: (loc -> Doc e) -> ReportError loc -> Doc e
printReportError _ (REMissingSections s) =
  "The following sections were missing:" `above` indent 1 (vcat $ map (pretty . unSN) (S.toList s))
printReportError pl (REDuplicateSection s l1 l2) =
  "The section" <+> pretty (unSN s) <+> "occurs twice:"
  `above` indent 1 (vcat $ map pl [l1, l2])
printReportError pl (REUnknownSection s l) =
  "The section" <+> pretty (unSN s) <+> "is unknown, at:"
  `above` indent 1 (pl l)
printReportError pl (RESectionError sn se) =
  "Section " <+> pretty (unSN sn) <+> "reports:" `above` indent 1 (vcat $ map (printSectionError pl) se)

printGrade :: Double -> Double -> Doc e
printGrade e t = brackets (p e <> "/" <> p t) <+> parens ((p $ e / t * 100.0) <> "%")
 where
  p x = text $ showFFloat (Just 1) x ""

printSection :: ReportFileSection -> Doc e
printSection (RFS st ss smax sdc sgc) =
  pretty st <> ":" <+> printGrade ss smax <> line
  `above` indent 1
    (vcat (punctuate empty (map pretty sdc))
     <> (if null sdc then empty else line) <> magc sgc)
 where
  magc x = if T.any (not . C.isSpace) x then agc x else empty
  agc x = "Additional Grader Comments:" <> line `above` indent 1 (pretty x)

total :: ReportFile -> (Double,Double)
total (RF secs) = foldr (\(RFS _ ss sm _ _) (e,t) -> (ss+e,sm+t)) (0.0,0.0) secs

printReport :: ReportFile -> Doc e
printReport r@(RF s) = vcat (map printSection s) <> line <> "TOTAL:" <+> (uncurry printGrade $ total r)
