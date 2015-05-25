-- Typechecking expressions
-- and other invariants
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Check (
      -- Perform all checks
      checkExp
      -- Perform all checks with an empty environment
    , checkExp0
      -- Do normal type checking
    , typecheck
    ) where

import              Icicle.Common.Type
import              Icicle.Common.Exp.Compounds
import              Icicle.Common.Exp.Exp
import              Icicle.Common.Exp.Error
import              Icicle.Common.Fragment

import              P

import qualified    Data.Map as Map


-- | Perform type checking and invariant checking with empty environment
checkExp0 :: (Ord n) => Fragment p -> Exp n p -> Either (ExpError n p) Type
checkExp0 frag x = checkExp frag Map.empty x


-- | Perform type checking and invariant checking with given environment
-- Invariants:
--  - normal type check
--  - names must be unique
--  - primitives must be fully applied
--
-- If successful, returns type of expression
--
checkExp :: (Ord n) => Fragment p -> Env n Type -> Exp n p -> Either (ExpError n p) Type
checkExp frag e x
 = do   -- Perform normal type checking first
        t <- typecheck frag e x

        -- Check other invariants
        
        -- Primitives must be fully applied
        case primsFullyApplied frag of
          True  -> checkPrimsFullyApplied frag x
          False -> return ()

        case allowLambdas frag of
          AllowLambdas                 -> return ()
          AllowLambdasAsPrimArgs       -> checkLambdasAllowed False x
          AllowLambdasAsPrimArgsAndTop -> checkLambdasAllowed True  x

        return t


-- | Typecheck expression, returning type if successful.
-- Also checks name uniqueness invariant while building up environment.
typecheck :: (Ord n) => Fragment p -> Env n Type -> Exp n p -> Either (ExpError n p) Type
typecheck frag e xx
 = case xx of
    -- Variable must exist
    XVar n
     -> lookupOrDie ExpErrorVarNotInEnv e n

    XValue t v
     -> if   valueMatchesType v t
        then return (FunT [] t)
        else Left (ExpErrorValueNotOfType v t)

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
     ->     return (typeOfPrim frag p)

    -- Abstraction
    XLam n t x
     -> do  e' <- insertOrDie ExpErrorNameNotUnique e n (FunT [] t)
            tt <- typecheck frag e' x
            return (funOfVal t `arrow` tt)

    -- Let binding
    XLet n x i
     -> do  x' <- go x
            e' <- insertOrDie ExpErrorNameNotUnique e n x'
            typecheck frag e' i

 where
  -- Descend into new expression with same environment
  go = typecheck frag e



-- | Check if all primitives are fully applied.
-- Only checks the number of arguments; the types are handled above.
checkPrimsFullyApplied :: (Ord n) => Fragment p -> Exp n p -> Either (ExpError n p) ()
checkPrimsFullyApplied frag xx
 = case xx of
    -- Variables are ok
    XVar{}
     -> ok

    XValue{}
     -> ok

    -- Application to primitive:
    -- recursively check all arguments, and check that number of arguments is fine
    XApp{}
     | Just (p,as) <- takePrimApps xx
     -> mapM_ go as
     >> check p (length as)

    -- Non-primitive application; recurse
    XApp p q
     -> go p
     >> go q

    -- Non-applied primitive, so better not take arguments
    XPrim p
     -> check p 0

    -- Abstraction
    XLam _ _ x
     -> go x

    -- Let binding
    XLet _ d i
     -> go d
     >> go i

 where
  -- Recurse with same fragment
  go = checkPrimsFullyApplied frag

  ok = return ()

  -- Get type of primitive and check how many arguments it takes against how many it got
  check p n
   = case typeOfPrim frag p of
      FunT args _
       | length args == n
       -> ok
       | otherwise
       -> Left $ ExpErrorPrimitiveNotFullyApplied p xx


-- | Check if lambdas are only in allowed placed
checkLambdasAllowed :: (Ord n) => Bool -> Exp n p -> Either (ExpError n p) ()
checkLambdasAllowed allowedHere xx
 = case xx of
    -- Variables are ok
    XVar{}
     -> ok

    XValue{}
     -> ok

    -- Application to primitive:
    -- recursively check all arguments, and check that number of arguments is fine
    XApp{}
     | Just (_,as) <- takePrimApps xx
     -> mapM_ (checkLambdasAllowed True) as

    -- Non-primitive application; recurse but no lams allowed
    XApp p q
     -> go p
     >> go q

    -- Non-applied primitive, ok
    XPrim _
     -> ok

    -- Abstraction
    XLam _ _ x
     | allowedHere
     -> checkLambdasAllowed allowedHere x
     | otherwise
     -> err

    -- Let binding
    XLet _ d i
     -> go d
     >> go i

 where
  -- Recurse, not at top level any more
  go = checkLambdasAllowed False
  ok = return ()
  err = Left $ ExpErrorLambdaNotAllowedHere xx

