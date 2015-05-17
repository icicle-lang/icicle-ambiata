-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Common.Base
import              Icicle.Common.Exp

import              Icicle.Internal.Pretty

import              P

-- | An entire Avalanche program
data Program n p =
  Program
  { binddate    :: Name n
  , statements  :: Statement n p
  }
 deriving (Eq, Ord, Show)

instance TransformX Program where
 transformX names exps p
  = do  binddate'   <-      names                  $ binddate   p
        statements' <-      transformX names exps  $ statements p

        return $ Program 
               { binddate   = binddate'
               , statements = statements'
               }

-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program n p) where
 pretty p
  =   pretty (binddate   p) <> text " = DATE" <> line
  <>  pretty (statements p)


