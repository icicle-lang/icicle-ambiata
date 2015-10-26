-- | Reductions over a stream
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Reduce.Reduce (
      Reduce     (..)
    , renameReduce
    , inputOfReduce
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp
import              Icicle.Common.Exp.Exp (renameExp)

import              P



data Reduce a n
 = RFold   ValType ValType (Exp a n) (Exp a n) (Name n)
 deriving (Eq,Ord,Show)


renameReduce :: (Name n -> Name n') -> Reduce a n -> Reduce a n'
renameReduce f (RFold t a k z n) = RFold t a (renameExp f k) (renameExp f z) (f n)


-- | Get name of input stream for given reduction
inputOfReduce :: Reduce a n -> Name n
inputOfReduce (RFold _ _ _ _ inp) = inp


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Reduce a n) where
 pretty (RFold t a k z n)
  =   annotate (AnnType $ (pretty t) <+> (pretty a)) "rfold"
  </> parens (pretty k) </> parens (pretty z) </> pretty n

