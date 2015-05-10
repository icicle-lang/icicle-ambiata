{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Stream.Error (
      StreamError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp

import              P

data StreamError n
 -- No such stream variable
 = StreamErrorVarNotInEnv   (Name n)
 -- Worker function doesn't type check
 | StreamErrorExp           (ExpError n Prim)
 -- can't do something with worker function
 | StreamErrorTypeError     (Exp n) Type Type
 deriving Show

instance (Pretty n) => Pretty (StreamError n) where
 pretty e
  = case e of
    StreamErrorVarNotInEnv n
     -> text "Stream variable not bound: " <> pretty n

    StreamErrorExp err
     ->  text "Worker function type error: "
     <+> indent 4 (pretty err)

    StreamErrorTypeError x t y
     ->  text "Worker function type mismatch:"
     <+> indent 4 (text "Exp:      " <> pretty x <> line
                <> text "Expected: " <> pretty t <> line
                <> text "Actual:   " <> pretty y)

