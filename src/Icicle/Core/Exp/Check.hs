-- Typechecking expressions
-- and other invariants
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Exp.Check (
      -- Perform all checks
      checkExp
      -- Do normal type checking
    , typecheck
      -- All primitives must be fully applied
    , primsFullyApplied
    ) where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Exp
import              Icicle.Core.Exp.Error
import              Icicle.Core.Exp.Prim

import              P

import qualified    Data.Map as Map

type Env n = Map.Map (Name n) Type


checkExp :: (Ord n) => Exp n -> Either (CheckError n) Type
checkExp x
 = do   t <- typecheck Map.empty x
        _ <- primsFullyApplied x
        return t


typecheck :: (Ord n) => Env n -> Exp n -> Either (CheckError n) Type
typecheck e xx
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
            typecheck e' x

    XLet n x i
     -> do  x' <- go x
            e' <- insertOrDie e n x'
            typecheck e' i
            
 where
  go = typecheck e


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


primsFullyApplied :: (Ord n) => Exp n -> Either (CheckError n) ()
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
       -> Left $ CheckErrorPrimitiveNotFullyApplied p xx

