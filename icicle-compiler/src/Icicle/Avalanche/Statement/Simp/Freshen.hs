{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE BangPatterns      #-}
module Icicle.Avalanche.Statement.Simp.Freshen (
    freshen
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              P

import              Data.Set (Set)
import qualified    Data.Set as Set
import              Data.Map.Strict (Map)
import qualified    Data.Map.Strict as Map
import              Data.Hashable (Hashable)


-- | Freshen names as required
--
freshen :: (Hashable n, Eq n) => a -> Statement a n p -> Fresh n (Statement a n p)
freshen = freshenS mempty mempty

freshenS :: (Hashable n, Eq n) => Set (Name n) -> Map (Name n) (Exp a n p) -> a -> Statement a n p -> Fresh n (Statement a n p)
freshenS us payload a_fresh statements
 = case statements of
    If x ts fs
     -> If <$> goX x <*> go ts <*> go fs
    Let n x ss
     -> do (n', go') <- insertName n
           Let n' <$> goX x <*> go' ss

    While t n nt end ss
     -> do (n', go') <- insertName n
           While t n' nt <$> goX end <*> go' ss
    ForeachInts t n from to ss
     -> do (n', go') <- insertName n
           ForeachInts t n' <$> goX from <*> goX to <*> go' ss
    -- TODO
    ForeachFacts binds vt ty ss
     -> ForeachFacts binds vt ty <$> go ss

    Block ss
     -> Block <$> mapM go ss

    InitAccumulator (Accumulator n t x) ss
     -> InitAccumulator <$> (Accumulator n t <$> goX x) <*> go ss

    Read nx na t ss
     -> do (n', go') <- insertName nx
           Read n' na t <$> go' ss

    Write na x
     -> Write na <$> goX x

    Output n t xts
     -> Output n t <$> mapM (\(x',t') -> (,) <$> goX x' <*> return t') xts
    KeepFactInHistory x
     -> KeepFactInHistory <$> goX x

    LoadResumable{}
     -> return statements
    SaveResumable{}
     -> return statements

 where
  goX x
   | Map.null payload
   = return x
   | otherwise
   = subst (annotOfExp x) payload x

  go = freshenS us payload a_fresh

  insertName n
   | Set.member n us
   = do n' <- fresh
        let payload' = Map.insert n (XVar a_fresh n') payload
        return (n', freshenS us payload' a_fresh)
   | otherwise
   = do return (n, freshenS (Set.insert n us) payload a_fresh)

