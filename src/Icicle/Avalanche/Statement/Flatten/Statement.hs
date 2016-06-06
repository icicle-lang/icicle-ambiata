-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten.Statement (
    flatten
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base
import              Icicle.Avalanche.Statement.Flatten.Exp
import              Icicle.Avalanche.Statement.Flatten.Type
import              Icicle.Avalanche.Statement.Flatten.Save


import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat

import qualified    Icicle.Core.Exp.Prim           as Core

import              Icicle.Internal.Pretty

import              P

import qualified    Data.List                      as List
import              Data.Hashable                  (Hashable)
import              Data.String                    (IsString)

flatten :: (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> Statement a n Core.Prim
        -> FlatM a n
flatten a_fresh s = flattenS a_fresh [] s

-- Extracting FactIdentifiers from Buffers:
--
--
-- After the end of the ForeachFacts loop, we need to go through the FactIdentifier buffers and
-- call KeepFactInHistory for each FactIdentifier.
-- This is a bit trickier than you might think:
--  1. Bufs might be nested inside other places, like inside Maps or inside tuples
--  2. We won't necessarily read from all the bufs, but bufs we don't read might be important next time
--    (Imagine we have a Map of Bufs, and we choose one entry to read the values from and return.
--     The next time we run, we might not choose the same entry, so we need to make sure all the Bufs are saved)
--
-- So, we need to modify flatten to keep track of all Accumulators in scope (or perhaps only accumulators with nested Buf types).
-- For each accumulator, we need to generate code for traversing the structure and finding the nested Bufs.
-- When we find the Buf, we can read the fact identifiers and mark them as necessary.
--
-- (It might be worth storing each accumulator's original type, rather than the modified/tupled type,
-- as searching for a Buf is probably easier than searching for the first element in tuple of two bufs)

-- | Flatten the primitives in a statement.
-- This just calls @flatX@ for every expression, wrapping the statement.
flattenS :: (Pretty n, Hashable n, Eq n, IsString n, Show n, Show a)
        => a
        -> [Accumulator a n Flat.Prim]
        -> Statement a n Core.Prim
        -> FlatM a n
flattenS a_fresh accums s
 = case s of
    If x ts es
     -> flatX a_fresh x
     $ \x'
     -> If x' <$> flattenS a_fresh accums ts <*> flattenS a_fresh accums es

    Let n x ss
     -> flatX a_fresh x
     $ \x'
     -> Let n x' <$> flattenS a_fresh accums ss

    While t n to ss
     -> flatX a_fresh to
     $ \to'
     -> While t n to' <$> flattenS a_fresh accums ss

    ForeachInts t n from to ss
     -> flatX a_fresh from
     $ \from'
     -> flatX a_fresh to
     $ \to'
     -> ForeachInts t n from' to' <$> flattenS a_fresh accums ss

    ForeachFacts binds vt lo ss
     -- Input binds cannot contain Buffers, so no need to flatten the types
     -> do  loop  <- ForeachFacts binds (flatT vt) lo <$> flattenS a_fresh accums ss
            -- Run through all the accumulators and save the buffers
            save  <- mapM (flattenSaveAccumulator a_fresh) accums
            return $ mconcat (loop : save)

    Block ss
     -> Block <$> mapM (flattenS a_fresh accums) ss

    InitAccumulator acc ss
     -> flatX a_fresh (accInit acc)
     $ \x'
     -> let acc' = acc
                 { accInit = x'
                 , accValType = flatT (accValType acc)}
        in InitAccumulator acc'
        <$> flattenS a_fresh (acc':accums) ss

    Read n m vt ss
     -> Read n m (flatT vt) <$> flattenS a_fresh accums ss

    Write n x
     -> flatX a_fresh x (return . Write n)

    Output n t xts
     | xs <- fmap fst xts
     , ts <- fmap (flatT.snd) xts
     -> flatXS a_fresh xs []
     $ \xs'
     -> return $ Output n (flatT t) (List.zip xs' ts)

    KeepFactInHistory x
     -> flatX a_fresh x (return . KeepFactInHistory)

    LoadResumable n t
     -> return $ LoadResumable n (flatT t)
    SaveResumable n t
     -> return $ SaveResumable n (flatT t)


