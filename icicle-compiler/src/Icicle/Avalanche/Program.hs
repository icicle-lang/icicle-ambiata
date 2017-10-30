-- | Avalanche programs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  , renameProgram
  ) where

import              GHC.Generics (Generic)

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P


-- | An entire Avalanche program
data Program a n p =
  Program
  { input       :: !ValType
  , bindtime    :: !(Name n)
  , maxMapSize  :: !(Name n)
  , statements  :: !(Statement a n p)
  }
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n, NFData p) => NFData (Program a n p)

instance TransformX Program where
 transformX names exps p
  = do  bindtime'   <-      names                  $ bindtime   p
        maxMapSize' <-      names                  $ maxMapSize p
        statements' <-      transformX names exps  $ statements p

        return $ Program
               { input      = input p
               , bindtime   = bindtime'
               , maxMapSize = maxMapSize'
               , statements = statements'
               }

renameProgram :: (Name n -> Name n') -> Program a n p -> Program a n' p
renameProgram f p
  = p { bindtime   = f          $ bindtime   p
      , maxMapSize = f          $ maxMapSize p
      , statements = renameStmt $ statements p}
  where
    renameFactBinds (FactBinds t vs)
      = FactBinds (f t) (fmap (first f) vs)

    renameAcc (Accumulator n t x)
      = Accumulator (f n) t (renameExp f x)

    renameStmt xx = case xx of
      If x s1 s2
        -> If (renameExp f x) (renameStmt s1) (renameStmt s2)
      Let n x s
        -> Let (f n) (renameExp f x) (renameStmt s)
      While t n nt x s
        -> While t (f n) nt (renameExp f x) (renameStmt s)
      ForeachInts t n x1 x2 s
        -> ForeachInts t (f n) (renameExp f x1) (renameExp f x2) (renameStmt s)
      ForeachFacts bs vt s
        -> ForeachFacts (renameFactBinds bs) vt (renameStmt s)
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

-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program a n p) where
  pretty p =
    vsep [
        prettyBinding
          (prettyTypedFlat (pretty $ bindtime p) (pretty TimeT))
          (annotate AnnConstant $ text "TIME")
      , prettyBinding
          (prettyTypedFlat (pretty $ maxMapSize p) (pretty IntT))
          (annotate AnnConstant $ text "MAX_MAP_SIZE")
      , mempty
      , pretty (statements p)
      ]

