{-# LANGUAGE OverloadedStrings #-}

module Grade.Skeleton where

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
  $ \(sn, ExSec (Sec _ _ shidden scl _ sds msh)) ->
    let scl' = interpSectionComments shidden scl in
    if shidden
     then scl'
     else scl'
          `above` "@" <> pretty (unSN sn) <> maybe empty ((empty <+>) . pretty) (fst msh)
          `above` prettyDings sds (vcat [empty, commentStart, empty, commentEnd])

 where
  prettyDings [] = id
  prettyDings ds = (indent 1 (vcat $ map prettyDing ds) `above`)

  prettyDing (dn, DingDefn _ _ mult dcl) =
    (if not (null dcl)
     then (above (vcat (map pretty dcl)) . indent 1)
     else id)
    $ "#:" <> pretty (unDN dn)
           <> (if mult then " # repeat as needed" else empty)
