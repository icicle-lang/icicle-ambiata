{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Stream.Stream (
      Stream          (..)
    , renameStream
    , isStreamWindowed
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp
import              Icicle.Common.Exp.Exp (renameExp)

import              P


data Stream a n
 = SFold    !(Name n) !ValType !(Exp a n) !(Exp a n)
 | SFilter           !(Exp a n) [Stream a n]
 -- SWindow !ValType !WindowUnit !(Maybe WindowUnit) !(Name n)
 deriving (Eq,Ord,Show)


renameStream :: (Name n -> Name n') -> Stream a n -> Stream a n'
renameStream f (SFold n t x y)        = SFold (f n) t (renameExp f x) (renameExp f y)
renameStream f (SFilter x ss)         = SFilter (renameExp f x) (fmap (renameStream f) ss)


-- | Check if given stream originates from a windowed or not
isStreamWindowed :: Eq n => [(Name n, Stream a n)] -> Name n -> Bool
isStreamWindowed _ss _nm
 -- XXX TODO: fix this
 = False


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Stream a n) where
 pretty (SFold n t z k)
  = padDoc 20 (pretty n)
  <> " = "
  <> "sfold [" <> pretty t <> "] (" <> pretty z <> ") then (" <> pretty k <> ")"
 pretty (SFilter x ss) = "sfilter " <> pretty x <> line
                      <> indent 2 (vsep $ fmap pretty ss)

