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


-- | Perform type checking and invariant checking with empty environment
checkExp0 :: (Ord n) => Exp n -> Either (ExpError n) Type
checkExp0 = checkExp Map.empty


-- | Perform type checking and invariant checking with given environment
-- Invariants:
--  - normal type check
--  - names must be unique
--  - primitives must be fully applied
--
-- If successful, returns type of expression
--
checkExp :: (Ord n) => Env n Type -> Exp n -> Either (ExpError n) Type
checkExp e x
 = do   t <- typecheck e x

        _ <- primsFullyApplied x
        return t


-- | Typecheck expression, returning type if successful.
-- Also checks name uniqueness invariant while building up environment.
typecheck :: (Ord n) => Env n Type -> Exp n -> Either (ExpError n) Type
typecheck e xx
 = case xx of
    -- Variable must exist
    XVar n
     -> lookupOrDie ExpErrorVarNotInEnv e n

    -- Application
    XApp p q
     -> do  p' <- go p
            q' <- go q
            -- Check if type of p accepts q as an argument
            case canApply p' q' of
             Just r   -> return r
             Nothing  -> Left (ExpErrorApp p q p' q')

    -- Look up primitive type
    XPrim p
     ->     return (typeOfPrim p)

    -- Abstraction
    XLam n t x
     -> do  e' <- insertOrDie ExpErrorNameNotUnique e n (FunT [] t)
            tt <- typecheck e' x
            return (funOfVal t `arrow` tt)

    -- Let binding
    XLet n x i
     -> do  x' <- go x
            e' <- insertOrDie ExpErrorNameNotUnique e n x'
            typecheck e' i

 where
  -- Descend into new expression with same environment
  go = typecheck e



-- | Check if all primitives are fully applied.
-- Only checks the number of arguments; the types are handled above.
primsFullyApplied :: (Ord n) => Exp n -> Either (ExpError n) ()
primsFullyApplied xx
 = case xx of
    -- Variables are ok
    XVar{}
     -> ok

    -- Application to primitive:
    -- recursively check all arguments, and check that number of arguments is fine
    XApp{}
     | Just (p,as) <- takePrimApps xx
     -> mapM_ primsFullyApplied as
     >> check p (length as)

    -- Non-primitive application; recurse
    XApp p q
     -> primsFullyApplied p
     >> primsFullyApplied q

    -- Non-applied primitive, so better not take arguments
    XPrim p
     -> check p 0

    -- Abstraction
    XLam _ _ x
     -> primsFullyApplied x

    -- Let binding
    XLet _ d i
     -> primsFullyApplied d
     >> primsFullyApplied i

 where
  ok = return ()

  -- Get type of primitive and check how many arguments it takes against how many it got
  check p n
   = case typeOfPrim p of
      FunT args _
       | length args == n
       -> ok
       | otherwise
       -> Left $ ExpErrorPrimitiveNotFullyApplied p xx

