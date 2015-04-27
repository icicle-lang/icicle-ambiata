{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Type (
      ValType (..)
    , FunType (..)
    , Type
    , funOfVal

    , Env
    , lookupOrDie
    , insertOrDie

    , canApply
    , requireSame

    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base

import              P

import qualified    Data.Map as Map


data ValType =
   IntT
 | BoolT
 | ArrayT ValType
 | PairT  ValType ValType
 deriving (Eq,Ord,Show)
 -- | Struct | String | Double | ...

data FunType =
 FunT [FunType] ValType
 deriving (Eq,Ord,Show)

-- Top-level type
type Type = FunType

funOfVal :: ValType -> FunType
funOfVal = FunT []


canApply :: Type -> Type -> Maybe Type
canApply (FunT args p) q
 = case args of
    (b:bs)
     | b == q
     -> Just (FunT bs p)
    _
     -> Nothing

type Env n t = Map.Map (Name n) t


lookupOrDie :: Ord n => (Name n -> err) -> Env n t -> Name n -> Either err t
lookupOrDie err e n
 = maybeToRight
        (err n)
        (Map.lookup n e)


-- Insert unique
insertOrDie :: Ord n => (Name n -> err) -> Env n t -> Name n -> t -> Either err (Env n t)
insertOrDie err e n t
 = case Map.lookup n e of
    Just _
     -> Left   $ err n
    _
     -> return $ Map.insert n t e


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
 pretty (ArrayT t)      = text "[" <> pretty t <> text "]"
 pretty (PairT a b)     = text "(" <> pretty a <> text ", " <> pretty b <> text ")"


instance Pretty FunType where
 pretty (FunT [] t)     = pretty t
 pretty (FunT (b:bs) t) = inner b <> text " -> " <> pretty (FunT bs t)
  where
   inner i@(FunT [] _) = pretty i
   inner i             = text "(" <> pretty i <> text ")"


