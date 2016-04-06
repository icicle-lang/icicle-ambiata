-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  , renameProgram
  ) where

import              Icicle.Avalanche.Statement.Statement
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

renameProgram :: (Name n -> Name n') -> Program a n p -> Program a n' p
renameProgram f p
  = p { bindtime   = f          $ bindtime p
      , statements = renameStmt $ statements p}
  where
    renameFactBinds (FactBinds t i vs)
      = FactBinds (f t) (f i) (fmap (first f) vs)

    renameAcc (Accumulator n t x)
      = Accumulator (f n) t (renameExp f x)

    renameStmt xx = case xx of
      If x s1 s2
        -> If (renameExp f x) (renameStmt s1) (renameStmt s2)
      Let n x s
        -> Let (f n) (renameExp f x) (renameStmt s)
      ForeachInts n x1 x2 s
        -> ForeachInts (f n) (renameExp f x1) (renameExp f x2) (renameStmt s)
      ForeachFacts bs vt ft s
        -> ForeachFacts (renameFactBinds bs) vt ft (renameStmt s)
      Block s
        -> Block (fmap renameStmt s)
      InitAccumulator acc s
        -> InitAccumulator (renameAcc acc) (renameStmt s)
      Read n1 n2 t s
        -> Read (f n1) (f n2) t (renameStmt s)
      Write n x
        -> Write (f n) (renameExp f x)
      Output o t xs
        -> Output o t (fmap (first (renameExp f)) xs)
      KeepFactInHistory x
        -> KeepFactInHistory (renameExp f x)
      LoadResumable n t
        -> LoadResumable (f n) t
      SaveResumable n t
        -> SaveResumable (f n) t


-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program a n p) where
 pretty p
  =   pretty (bindtime   p) <> text " = TIME" <> line
  <>  pretty (statements p)


