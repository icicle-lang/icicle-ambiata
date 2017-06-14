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

import              Control.Monad.Trans.Class (lift)
import              Control.Monad.Trans.State


-- | Freshen names as required
-- Set Name     - all seen names, including in other binding contexts - not just shadowing.
-- Map Name Exp - current substitution, just shadowing
freshen :: (Hashable n, Eq n) => a -> Statement a n p -> Fresh n (Statement a n p)
freshen a s = evalStateT (freshenS mempty a s) mempty

freshenS :: (Hashable n, Eq n) => Map (Name n) (Exp a n p) -> a -> Statement a n p -> StateT (Set (Name n)) (Fresh n) (Statement a n p)
freshenS payload a_fresh statements
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

    ForeachFacts (FactBinds fbTime fbId fbValues) vt ty ss
     -> do (fbTime',   payload1) <- insertName'     fbTime   payload
           (fbId',     payload2) <- insertName'     fbId     payload1
           (fbValues', payload3) <- insertFactBinds fbValues payload2
           ForeachFacts (FactBinds fbTime' fbId' fbValues') vt ty <$> freshenS payload3 a_fresh ss

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
   = lift $ subst (annotOfExp x) payload x

  go = freshenS payload a_fresh

  insertFactBinds [] p = return ([], p)
  insertFactBinds ((n,t):fbs) p = do
   (n',p')  <- insertName' n p
   (fbs',p'') <- insertFactBinds fbs p'
   return ((n',t):fbs', p'')

  insertName n = do
    (n',payload') <- insertName' n payload
    return (n', freshenS payload' a_fresh)

  insertName' n payload0 = do
    us <- get
    case Set.member n us of
     True -> do
      n' <- lift fresh
      let payload' = Map.insert n (XVar a_fresh n') payload0
      return (n', payload')
     False -> do
      put (Set.insert n us)
      return (n, payload0)

