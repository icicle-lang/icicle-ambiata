-- | Reductions over a stream
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Reduce (
      Reduce     (..)
    , renameReduce
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp
import              Icicle.Common.Exp.Exp (renameExp)

import              P



data Reduce n
 = RFold   ValType ValType (Exp n) (Exp n) (Name n)
 | RLatest ValType         (Exp n)         (Name n)
 deriving (Eq,Ord,Show)


renameReduce :: (Name n -> Name n') -> Reduce n -> Reduce n'
renameReduce f (RFold t a k z n) = RFold t a (renameExp f k) (renameExp f z) (f n)
renameReduce f (RLatest t   x n) = RLatest t                 (renameExp f x) (f n)


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Reduce n) where
 pretty (RFold t a k z n)
  =   text "rfold  "
  <+> text "[" <> pretty t <> text "]"
  <+> text "[" <> pretty a <> text "]"
  <+> parens (pretty k) <+> parens (pretty z) <+> pretty n
 pretty (RLatest t x n)
  =   text "rlatest"
  <+> text "[" <> pretty t <> text "]"
  <+> parens (pretty x) <+> pretty n

