-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Icicle.Avalanche.Statement.Flatten.Type (
    flatT
  , flatFT
  , flatPrimMinimal
  ) where

import              Icicle.Common.Type
import              Icicle.Common.Exp.Prim.Minimal

import qualified    Data.Map as Map
import              P


flatFT  :: FunType
        -> FunType
flatFT (FunT args ret)
 = FunT (fmap flatFT args) (flatT ret)

flatT   :: ValType
        -> ValType
flatT ot
 = case ot of
    BoolT       -> ot
    TimeT       -> ot
    DoubleT     -> ot
    IntT        -> ot
    StringT     -> ot
    UnitT       -> ot
    ErrorT      -> ot
    FactIdentifierT-> ot
    
    ArrayT t    -> ArrayT $ flatT t
    MapT t u    -> MapT  (flatT t) (flatT u)
    OptionT t   -> OptionT $ flatT t
    PairT t u   -> PairT (flatT t) (flatT u)
    SumT  t u   -> SumT  (flatT t) (flatT u)
    StructT ss  -> StructT $ flatTS ss

    BufT a t -> PairT (BufT a FactIdentifierT) (BufT a $ flatT t)

flatTS :: StructType -> StructType
flatTS = StructType . Map.map flatT . getStructType

flatPrimMinimal :: Prim -> Prim
flatPrimMinimal prim
 = case prim of
    PrimRelation r t -> PrimRelation r (flatT t)
    PrimConst p'     -> PrimConst $ primConst  p'
    PrimPair  p'     -> PrimPair  $ primPair   p'
    PrimStruct p'    -> PrimStruct$ primStruct p'
    _                -> prim

 where
  primConst (PrimConstPair  a b) = PrimConstPair  (flatT a) (flatT b)
  primConst (PrimConstSome  a)   = PrimConstSome  (flatT a)
  primConst (PrimConstLeft  a b) = PrimConstLeft  (flatT a) (flatT b)
  primConst (PrimConstRight a b) = PrimConstRight (flatT a) (flatT b)

  primPair  (PrimPairFst    a b) = PrimPairFst    (flatT a) (flatT b)
  primPair  (PrimPairSnd    a b) = PrimPairSnd    (flatT a) (flatT b)

  primStruct(PrimStructGet f t st)
                                 = PrimStructGet f (flatT t) (flatTS st)

