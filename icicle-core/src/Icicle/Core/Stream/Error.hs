{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Stream.Error (
      StreamError (..)
    ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Core.Exp

import           P

data StreamError a n
 -- No such stream variable
 = StreamErrorNameNotUnique   (Name n)
 -- Worker function doesn't type check
 | StreamErrorExp           (ExpError a n Prim)
 -- can't do something with worker function
 | StreamErrorTypeError     (Exp a n) Type Type
 deriving (Show, Generic)

instance (NFData a, NFData n) => NFData (StreamError a n)

instance (Pretty n) => Pretty (StreamError a n) where
 pretty e
  = case e of
    StreamErrorNameNotUnique n
     -> text "Stream variable not unique: " <> pretty n

    StreamErrorExp err
     ->  text "Worker function type error: "
     <> line
     <> indent 4 (pretty err)

    StreamErrorTypeError x t y
     ->  text "Worker function type mismatch:"
     <> line
     <>  indent 4 (text "Exp:      " <> pretty x <> line
                <> text "Expected: " <> pretty t <> line
                <> text "Actual:   " <> pretty y)

