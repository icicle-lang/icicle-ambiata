-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  ) where

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Statement.Scoped as Scoped
import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P

-- | An entire Avalanche program
data Program a n p =
  Program
  { input       :: ValType
  , bindtime    :: Name n
  , statements  :: Statement a n p
  }
 deriving (Eq, Ord, Show)

instance TransformX Program where
 transformX names exps p
  = do  bindtime'   <-      names                  $ bindtime   p
        statements' <-      transformX names exps  $ statements p

        return $ Program 
               { input      = input p
               , bindtime   = bindtime'
               , statements = statements'
               }

-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program a n p) where
 pretty p
  =   pretty (bindtime   p) <> text " = TIME" <> line
  <>  pretty (Scoped.scopedOfStatement $ statements p)


