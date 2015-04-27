{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Error (
      ReduceError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp

import              P

data ReduceError n
 -- No such stream variable
 = ReduceErrorNoSuchStream  (Name n)
 -- Worker function doesn't type check
 | ReduceErrorExp           (ExpError n)
 -- can't do something with worker function
 | ReduceErrorTypeError     (Exp n) Type Type
 deriving Show

instance (Pretty n) => Pretty (ReduceError n) where
 pretty e
  = case e of
    ReduceErrorNoSuchStream n
     -> text "No such stream: " <> pretty n

    ReduceErrorExp err
     ->  text "Worker function type error: "
     <+> indent 4 (pretty err)

    ReduceErrorTypeError x t y
     ->  text "Worker function type mismatch:"
     <+> indent 4 (text "Exp:      " <> pretty x <> line
                <> text "Expected: " <> pretty t <> line
                <> text "Actual:   " <> pretty y)

