{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Check (
      checkExp
    ) where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Exp
import              Icicle.Core.Exp.Error
import              Icicle.Core.Exp.Prim

import              P

import qualified    Data.Map as Map

type Env n = Map.Map (Name n) Type


checkExp :: (Ord n) => Env n -> Exp n -> Either (CheckError n) Type
checkExp e xx
 = case xx of
    XVar n
     -> lookupOrDie e n
    XApp p q
     -> do  p' <- go p
            q' <- go q
            case canApply p' q' of
             Just r   -> return r
             Nothing  -> Left (CheckErrorApp p q p' q')
    XPrim p
     ->     return (typeOfPrim p)

    XLam n t x
     -> do  e' <- insertOrDie e n (FunT [] t)
            checkExp e' x

    XLet n x i
     -> do  x' <- go x
            e' <- insertOrDie e n x'
            checkExp e' i
            
 where
  go = checkExp e


lookupOrDie :: Ord n => Env n -> Name n -> Either (CheckError n) Type
lookupOrDie e n
 = maybeToRight
        (CheckErrorVarNotInEnv n)
        (Map.lookup n e)


insertOrDie :: Ord n => Env n -> Name n -> Type -> Either (CheckError n) (Env n)
insertOrDie e n t
 = case Map.lookup n e of
    Just _
     -> Left   $ CheckErrorNameNotUnique n
    _
     -> return $ Map.insert n t e

