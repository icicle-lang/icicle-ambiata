-- | Core types.
-- There is a split between "value types" and "function types" to simplify code generation.
-- The only higher-order functions should be those passed to primitives.
--
-- We also have no parametric polymorphism or type arguments:
-- at the moment I'm betting that the only polymorphism will be in primitives,
-- so not putting any polymorphism in the primitives should make typechecking
-- and everything simpler.
--
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Type (
      ValType (..)
    , FunType (..)
    , Type
    , funOfVal
    , arrow

    , Env
    , lookupOrDie
    , insertOrDie

    , functionArguments
    , functionReturns
    , canApply
    , requireSame

    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base

import              P

import qualified    Data.Map as Map


-- | Real values.
-- No functions here, because we don't want higher order functions in the generated code.
-- This restriction should simplify code generation, because we won't need to
-- deal with lambda lifting arbitrary functions.
data ValType =
   IntT
 | BoolT
 | ArrayT ValType
 | MapT   ValType ValType
 | OptionT        ValType
 | PairT  ValType ValType
 -- I don't think it will be too much of a stretch to add more types later.
 -- | Struct | String | Double | ...
 deriving (Eq,Ord,Show)


-- | Function types.
-- These are the types of an entire expression, or types of arguments passed to primitives.
--
-- The list is the arguments of the function, and the return type must be a primitive value.
-- This means there can be no curried functions.
-- If there are no arguments, it is just a value - not a function.
--
-- We need the list of arguments to be FunTypes to express the types of primitives:
-- for example, array fold would have type
-- > FunT
-- >      [ FunT [b, a] b       ##    (b -> a -> b)
-- >      , FunT [] b           ##  -> b
-- >      , FunT [] (Array a)   ##  -> [a]
-- >      ]                     ##
-- >        b                   ##  -> b
--
data FunType =
 FunT [FunType] ValType
 deriving (Eq,Ord,Show)


-- | The top-level type of an expression can be a function type.
type Type = FunType


-- | Promote a value type to a zero-argument function type.
funOfVal :: ValType -> FunType
funOfVal = FunT []

-- | Construct a function type.
-- First is argument type, second is return type.
arrow :: FunType -> FunType -> FunType
arrow from (FunT args to)
 = FunT (from:args) to


-- |
functionArguments :: Type -> [Type]
functionArguments (FunT args _)
 = args

-- | Get final return type of function
functionReturns :: Type -> ValType
functionReturns (FunT _ r)
 = r

-- | Check if a function type can be applied to an argument.
-- If successful, returns the result type; otherwise Nothing.
canApply :: Type -> Type -> Maybe Type
canApply (FunT args p) q
 = case args of
    (b:bs)
     | b == q
     -> Just (FunT bs p)
    _
     -> Nothing


-- | Type environments: just a mapping from variable names to types.
-- Parametric in type because it could be a function type or a value type.
type Env n t = Map.Map (Name n) t


-- | Get from environment or return given error
lookupOrDie :: Ord n => (Name n -> err) -> Env n t -> Name n -> Either err t
lookupOrDie err e n
 = maybeToRight
        (err n)
        (Map.lookup n e)


-- | Attempt to insert a name into the environment.
-- However, core expressions cannot have name shadowing, so if it already exists return an error.
--
-- The rationale for disallowing shadowing is so:
--  - nobody will write in core, so we lose no expressivity;
--  - capture-avoiding substitution is hard to get right.
--
-- I think by disallowing shadowing and general lambdas, substitution should be a lot simpler.
--
insertOrDie :: Ord n => (Name n -> err) -> Env n t -> Name n -> t -> Either err (Env n t)
insertOrDie err e n t
 = case Map.lookup n e of
    Just _
     -> Left   $ err n
    _
     -> return $ Map.insert n t e


-- | Require two types to be equal, or return given error if not.
requireSame 
    :: (Type -> Type -> err)
    ->  Type -> Type -> Either err ()
requireSame err p q
 | p == q
 = return ()
 | otherwise
 = Left $ err p q


-- Pretty printing ---------------

instance Pretty ValType where
 pretty IntT            = text "Int"
 pretty BoolT           = text "Bool"
 pretty (ArrayT t)      = text "Array " <> pretty t
 pretty (MapT k v)      = text "Map" <+> pretty k <+> pretty v
 pretty (OptionT a)     = text "Option" <+> pretty a
 pretty (PairT a b)     = text "(" <> pretty a <> text ", " <> pretty b <> text ")"


instance Pretty FunType where
 pretty (FunT [] t)     = pretty t
 pretty (FunT (b:bs) t) = inner b <> text " -> " <> pretty (FunT bs t)
  where
   inner i@(FunT [] _) = pretty i
   inner i             = text "(" <> pretty i <> text ")"


