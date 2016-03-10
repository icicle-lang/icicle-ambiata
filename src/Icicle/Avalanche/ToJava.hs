{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.ToJava (
    programToJava
  ) where

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Program

import              Icicle.Common.Annot

import              Icicle.Internal.Pretty

import              P


programToJava :: (Show a, Show n, Pretty n, Ord n) => Program (Annot a) n Prim -> Doc
programToJava _
 = "import java.util.*;"
