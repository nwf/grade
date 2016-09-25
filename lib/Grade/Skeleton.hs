{-# LANGUAGE OverloadedStrings #-}

module Grade.Skeleton where

import qualified Data.Text                    as T
import           Text.PrettyPrint.Free
import           Grade.Types
import           Grade.Parse (commentStart, commentEnd)

interpComments :: Bool -> [T.Text] -> Maybe (Doc e)
interpComments f0 t0 = let r = go f0 t0 in if null r then Nothing else Just (vcat r)
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
    let ic = interpComments shidden in
    let scl' = ic scl in
    if shidden
     then maybe empty id scl'
     else maybe id above scl' $
                  "#@" <> pretty (unSN sn) <> maybe empty ((empty <+>) . pretty) (fst msh)
          `above` prettyDings ic sds (vcat [empty, "#" <> commentStart, empty, "#" <> commentEnd])

 where
  prettyDings _ [] = id
  prettyDings ic ds = (indent 1 (vcat $ map (prettyDing ic) ds) `above`)

  prettyDing ic (dn, DingDefn _ _ mult dcl) =
    maybe id (\c -> above c . indent 1) (ic dcl)
    $ "#:" <> pretty (unDN dn)
           <> (if mult then " # repeat as needed" else empty)
