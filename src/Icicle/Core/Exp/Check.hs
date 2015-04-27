-- Typechecking expressions
-- and other invariants
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Exp.Check (
      -- Perform all checks
      checkExp
      -- Perform all checks with an empty environment
    , checkExp0
      -- Do normal type checking
    , typecheck
      -- All primitives must be fully applied
    , primsFullyApplied
    ) where

import              Icicle.Core.Type
import              Icicle.Core.Exp.Exp
import              Icicle.Core.Exp.Error
import              Icicle.Core.Exp.Prim

import              P

import qualified    Data.Map as Map


checkExp0 :: (Ord n) => Exp n -> Either (ExpError n) Type
checkExp0 = checkExp Map.empty


checkExp :: (Ord n) => Env n Type -> Exp n -> Either (ExpError n) Type
checkExp e x
 = do   t <- typecheck e x
        _ <- primsFullyApplied x
        return t


typecheck :: (Ord n) => Env n Type -> Exp n -> Either (ExpError n) Type
typecheck e xx
 = case xx of
    XVar n
     -> lookupOrDie ExpErrorVarNotInEnv e n
    XApp p q
     -> do  p' <- go p
            q' <- go q
            case canApply p' q' of
             Just r   -> return r
             Nothing  -> Left (ExpErrorApp p q p' q')
    XPrim p
     ->     return (typeOfPrim p)

    XLam n t x
     -> do  e' <- insertOrDie ExpErrorNameNotUnique e n (FunT [] t)
            typecheck e' x

    XLet n x i
     -> do  x' <- go x
            e' <- insertOrDie ExpErrorNameNotUnique e n x'
            typecheck e' i
            
 where
  go = typecheck e



primsFullyApplied :: (Ord n) => Exp n -> Either (ExpError n) ()
primsFullyApplied xx
 = case xx of
    XVar{}
     -> ok

    XApp{}
     | Just (p,as) <- takePrimApps xx
     -> mapM_ primsFullyApplied as
     >> check p (length as)

    XApp p q
     -> primsFullyApplied p
     >> primsFullyApplied q

    XPrim p
     -> check p 0

    XLam _ _ x
     -> primsFullyApplied x

    XLet _ d i
     -> primsFullyApplied d
     >> primsFullyApplied i

 where
  ok = return ()

  check p n
   = case typeOfPrim p of
      FunT args _
       | length args == n
       -> ok
       | otherwise
       -> Left $ ExpErrorPrimitiveNotFullyApplied p xx

