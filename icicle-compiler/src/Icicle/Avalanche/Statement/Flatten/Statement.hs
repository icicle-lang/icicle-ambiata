-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten.Statement (
    flatten
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base
import              Icicle.Avalanche.Statement.Flatten.Exp


import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Statement.Simp.Freshen  as Freshen
import qualified    Icicle.Common.Fresh                      as Fresh

import qualified    Icicle.Core.Exp.Prim           as Core

import              Icicle.Internal.Pretty hiding (flatten)

import              P

import qualified    Data.List                      as List
import              Data.Hashable                  (Hashable)
import              Data.String                    (IsString)

flatten :: (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> Statement a n Core.Prim
        -> FlatM a n
flatten a_fresh s = do
  s'  <- flattenS a_fresh s
  s'' <- Fresh.runFreshIdentity $ Freshen.freshen a_fresh s'
  return s''

-- | Flatten the primitives in a statement.
-- This just calls @flatX@ for every expression, wrapping the statement.
flattenS :: (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> Statement a n Core.Prim
        -> FlatM a n
flattenS a_fresh s
 = case s of
    If x ts es
     -> flatX a_fresh x
     $ \x'
     -> If x' <$> flattenS a_fresh ts <*> flattenS a_fresh es

    Let n x ss
     -> flatX a_fresh x
     $ \x'
     -> Let n x' <$> flattenS a_fresh ss

    While t n nt to ss
     -> flatX a_fresh to
     $ \to'
     -> While t n nt to' <$> flattenS a_fresh ss

    ForeachInts t n from to ss
     -> flatX a_fresh from
     $ \from'
     -> flatX a_fresh to
     $ \to'
     -> ForeachInts t n from' to' <$> flattenS a_fresh ss

    ForeachFacts binds vt ss
     -- Input binds cannot contain Buffers, so no need to flatten the types
     -> ForeachFacts binds vt <$> flattenS a_fresh ss

    Block ss
     -> Block <$> mapM (flattenS a_fresh) ss

    InitAccumulator acc ss
     -> flatX a_fresh (accInit acc)
     $ \x'
     -> InitAccumulator acc { accInit = x' } <$> flattenS a_fresh ss

    Read n m vt ss
     -> Read n m vt <$> flattenS a_fresh ss

    Write n x
     -> flatX a_fresh x (return . Write n)

    Output n t xts
     | (xs,ts) <- List.unzip xts
     -> flatXS a_fresh xs []
     $ \xs'
     -> return $ Output n t (List.zip xs' ts)


