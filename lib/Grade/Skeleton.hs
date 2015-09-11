{-# LANGUAGE OverloadedStrings #-}

module Grade.Skeleton where

import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           Text.PrettyPrint.Free
import           Grade.Types
import           Grade.Parse (commentStart, commentEnd)

interpSectionComments :: Bool -> [T.Text] -> Doc e
interpSectionComments f0 = vcat . go f0
 where
  go _     []                             = []
  go _     ("#!noskip":bs)                = go False bs
  go _     ("#!reskip":bs)                = go f0 bs
  go False ("#!\\n":bs)                   = empty : go False bs
  go f     (b:bs) | "#!" `T.isPrefixOf` b = go f bs
  go False (b:bs)                         = pretty b : go False bs
  go True  (_:bs)                         = go True bs

makeSkel :: Defines f loc -> Doc e
makeSkel (Defs _ sl) =
  vcat $ punctuate line
  $ flip fmap sl
  $ \(sn, ExSec (Sec _ _ shidden scl sdm msh)) ->
    let scl' = interpSectionComments shidden scl in
    if shidden
     then scl'
     else scl'
          `above` "@" <> pretty (unSN sn) <> maybe empty ((empty <+>) . pretty) (fst msh)
          `above` prettyDingMap sdm (vcat [empty, commentStart, empty, commentEnd])

 where
  prettyDingMap dm = if M.null dm
                      then id
                      else (indent 1 (vcat $ map prettyDing (M.toList dm)) `above`)

  prettyDing (dn, _) = "#:" <> pretty (unDN dn)
