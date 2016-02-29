-- Typechecking expressions
-- and other invariants
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Check (
      -- Perform all checks and return the type.
      typeExp
      -- Perform all checks with an empty environment and return the type.
    , typeExp0
      -- Perform all checks and return the annotated expression.
    , checkExp
      -- Perform all checks with an empty environment and return the annotated expression.
    , checkExp0
    ) where

import              Icicle.Common.Annot
import              Icicle.Common.Type
import              Icicle.Common.Exp.Compounds
import              Icicle.Common.Exp.Exp
import              Icicle.Common.Exp.Error
import              Icicle.Common.Fragment

import              P

import qualified    Data.Map as Map
import              Data.Hashable (Hashable)


-- | Perform all checks with an empty environment and return the type.
typeExp0 :: (Hashable n, Eq n) => Fragment p -> Exp a n p -> Either (ExpError a n p) Type
typeExp0 frag x = typeExp frag Map.empty x


-- | Perform all checks and return the type.
typeExp :: (Hashable n, Eq n) => Fragment p -> Env n Type -> Exp a n p -> Either (ExpError a n p) Type
typeExp frag env x = fmap (annType . annotOfExp) (checkExp frag env x)


-- | Perform type checking and invariant checking with empty environment
checkExp0 :: (Hashable n, Eq n) => Fragment p -> Exp a n p -> Either (ExpError a n p) (Exp (Annot a) n p)
checkExp0 frag x = checkExp frag Map.empty x


-- | Perform type checking and invariant checking with given environment
-- Invariants:
--  - normal type check
--  - names must be unique
--  - primitives must be fully applied
--
-- If successful, returns type of expression
--
checkExp :: (Hashable n, Eq n) => Fragment p -> Env n Type -> Exp a n p -> Either (ExpError a n p) (Exp (Annot a) n p)
checkExp frag env x
 = do   -- Perform normal type checking first
        t <- typecheck frag env x

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
typecheck :: (Hashable n, Eq n) => Fragment p -> Env n Type -> Exp a n p -> Either (ExpError a n p) (Exp (Annot a) n p)
typecheck frag env xx
 = case xx of
    -- Variable must exist
    XVar a n
     -> do t <- lookupOrDie ExpErrorVarNotInEnv env n
           return (XVar (Annot t a) n)

    XValue a t v
     -> if   valueMatchesType v t
        then Right (XValue (Annot (FunT [] t) a) t v)
        else Left  (ExpErrorValueNotOfType v t)

    -- Application
    XApp a p q
     -> do  p' <- go p
            q' <- go q

            let pt = annType (annotOfExp p')
                qt = annType (annotOfExp q')

            -- Check if type of p accepts q as an argument
            case canApply pt qt of
             Just r   -> Right (XApp (Annot r a) p' q')
             Nothing  -> Left  (ExpErrorApp p q  pt qt)

    -- Look up primitive type
    XPrim a p
     ->     return (XPrim (Annot (typeOfPrim frag p) a) p)

    -- Abstraction
    XLam a n t x
     -> do  env' <- insertOrDie ExpErrorNameNotUnique env n (FunT [] t)
            x'   <- typecheck frag env' x

            let tt = annType (annotOfExp x')
                a' = Annot (funOfVal t `arrow` tt) a

            return (XLam a' n t x')

    -- Let binding
    XLet a n x i
     -> do  x'   <- go x

            let xt = annType (annotOfExp x')

            env' <- insertOrDie ExpErrorNameNotUnique env n xt
            i'   <- typecheck frag env' i

            let it = annType (annotOfExp i')
                a' = Annot it a

            return (XLet a' n x' i')

 where
  -- Descend into new expression with same environment
  go = typecheck frag env



-- | Check if all primitives are fully applied.
-- Only checks the number of arguments; the types are handled above.
checkPrimsFullyApplied :: (Hashable n, Eq n) => Fragment p -> Exp a n p -> Either (ExpError a n p) ()
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
    XApp _ p q
     -> go p
     >> go q

    -- Non-applied primitive, so better not take arguments
    XPrim _ p
     -> check p 0

    -- Abstraction
    XLam _ _ _ x
     -> go x

    -- Let binding
    XLet _ _ d i
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
checkLambdasAllowed :: (Hashable n, Eq n) => Bool -> Exp a n p -> Either (ExpError a n p) ()
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
    XApp _ p q
     -> go p
     >> go q

    -- Non-applied primitive, ok
    XPrim _ _
     -> ok

    -- Abstraction
    XLam _ _ _ x
     | allowedHere
     -> checkLambdasAllowed allowedHere x
     | otherwise
     -> err

    -- Let binding
    XLet _ _ d i
     -> go d
     >> go i

 where
  -- Recurse, not at top level any more
  go = checkLambdasAllowed False
  ok = return ()
  err = Left $ ExpErrorLambdaNotAllowedHere xx

