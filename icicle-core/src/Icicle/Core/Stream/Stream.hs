{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Stream.Stream (
      Stream          (..)
    , renameStream
    ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Core.Exp
import           Icicle.Common.Exp.Exp (renameExp)

import           P


data Stream a n
 = SFold    !(Name n)  !ValType !(Exp a n) !(Exp a n)
 | SFilter  !(Exp a n) ![Stream a n]
 -- SWindow !ValType !WindowUnit !(Maybe WindowUnit) !(Name n)
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (Stream a n)

renameStream :: (Name n -> Name n') -> Stream a n -> Stream a n'
renameStream f (SFold n t x y)        = SFold (f n) t (renameExp f x) (renameExp f y)
renameStream f (SFilter x ss)         = SFilter (renameExp f x) (fmap (renameStream f) ss)


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Stream a n) where
  pretty = \case
    SFold n t z k ->
      vsep [
          annotate AnnHeading "STREAM_FOLD"
            <+> prettyTypedFlat (annotate AnnBinding $ pretty n) (pretty t)
        , indent 2 $ vsep [
              annotate AnnHeading "INIT:"
            , indent 2 $ pretty z
            , annotate AnnHeading "KONS:"
            , indent 2 $ pretty k
            ]
        ]

    SFilter x ss ->
      vsep [
          annotate AnnHeading "STREAM_FILTER"
        , indent 2 $ vsep [
              annotate AnnHeading "PREDICATE:"
            , indent 2 $ pretty x
            , annotate AnnHeading "STREAMS:"
            , indent 2 . vsep $ fmap pretty ss
            ]
        ]
