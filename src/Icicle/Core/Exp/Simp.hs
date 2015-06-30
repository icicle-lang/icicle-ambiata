{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Core.Exp.Simp
     ( simp
     , simpX
     , simpP
     , simpMP
     ) where

import           Icicle.Common.Base
import           Icicle.Common.Exp              hiding (simp)
import qualified Icicle.Common.Exp.Prim.Minimal as M
import           Icicle.Common.Exp.Simp.ANormal
import qualified Icicle.Common.Exp.Simp.Beta    as B
import           Icicle.Common.Fresh
import           Icicle.Common.Type
import qualified Icicle.Core.Exp                as C
import           Icicle.Core.Exp.Prim

import           P


-- | Core Simplifier:
--   * a normal
--   * beta reduction
--   * constant folding for some primitives
--   * ...something exciting???
--
simp :: Ord n => (C.Exp n -> Bool) -> C.Exp n -> Fresh n (C.Exp n)
simp isValue = anormal . simpX isValue


simpX :: Ord n => (C.Exp n -> Bool) -> C.Exp n -> C.Exp n
simpX isValue = go
  where
    beta  = B.beta isValue
    go xx = case beta xx of
      -- * constant folding for some primitives
      XApp{}
        | Just (p, as) <- takePrimApps xx
        , Just args    <- sequenceA (fmap (takeValue . go) as)
        -> fromMaybe xx (simpP p args)

      -- * beta reduce primitive arguments
      XApp{}
        | Just (p, as) <- takePrimApps xx
        -> makeApps (XPrim p) (fmap go as)

      XApp p q
        -> XApp (go p) (go q)

      XLam n t x1
        -> XLam n t (go x1)

      XLet n x1 x2
        -> XLet n (go x1) (go x2)

      b@(XVar{})   -> b
      b@(XPrim{})  -> b
      b@(XValue{}) -> b


-- | Primitive Simplifier
--
simpP :: Prim -> [(ValType, BaseValue)] -> Maybe (C.Exp n)
simpP = go
  where
    go pp args = case pp of
      PrimMinimal mp  -> simpMP mp args
      -- * leaves fold and map alone for now..?
      PrimFold    _ _ -> Nothing
      PrimMap     _   -> Nothing

simpMP :: M.Prim -> [(ValType, BaseValue)] -> Maybe (C.Exp n)
simpMP = go
  where
    go pp args = case pp of
      -- * arithmetic on constant integers
      M.PrimArith M.PrimArithPlus
        -> arith2 pp args (+)
      M.PrimArith M.PrimArithMinus
        -> arith2 pp args (-)
      M.PrimArith M.PrimArithDiv
        -> Nothing -- arith2 pp args div
      M.PrimArith M.PrimArithMul
        -> arith2 pp args (*)
      M.PrimArith M.PrimArithNegate
        | [(_, VInt x)] <- args
        -> let t = functionReturns (M.typeOfPrim pp)
           in  return $ XValue t (VInt (-x))
        | otherwise -> Nothing

      -- * predicates on integers
      M.PrimRelation M.PrimRelationGt IntT
        -> int2 args (>)
      M.PrimRelation M.PrimRelationGe IntT
        -> int2 args (>=)
      M.PrimRelation M.PrimRelationLt IntT
        -> int2 args (<)
      M.PrimRelation M.PrimRelationLe IntT
        -> int2 args (<)
      M.PrimRelation M.PrimRelationEq IntT
        -> int2 args (==)
      M.PrimRelation M.PrimRelationNe IntT
        -> int2 args (/=)

      -- * predicates on bools
      M.PrimRelation M.PrimRelationGt BoolT
        -> bool2 args (>)
      M.PrimRelation M.PrimRelationGe BoolT
        -> bool2 args (>=)
      M.PrimRelation M.PrimRelationLt BoolT
        -> bool2 args (<)
      M.PrimRelation M.PrimRelationLe BoolT
        -> bool2 args (<)
      M.PrimRelation M.PrimRelationEq BoolT
        -> bool2 args (==)
      M.PrimRelation M.PrimRelationNe BoolT
        -> bool2 args (/=)

      M.PrimRelation _ _ -> Nothing

      -- * logical
      M.PrimLogical M.PrimLogicalAnd
        -> bool2 args (&&)
      M.PrimLogical M.PrimLogicalOr
        -> bool2 args (||)
      M.PrimLogical M.PrimLogicalNot
        -> bool1 args not

      -- * constructors
      M.PrimConst    (M.PrimConstPair _ _)
       | [(ta,va),(tb,vb)] <- args
       -> return $ XValue (PairT ta tb) (VPair va vb)
       | otherwise
       -> Nothing

      M.PrimConst    (M.PrimConstSome _)
       | [(ta,va)] <- args
       -> return $ XValue (OptionT ta) (VSome va)
       | otherwise
       -> Nothing

      -- * leaves datetime alone
      M.PrimDateTime _ -> Nothing

      M.PrimPair    (M.PrimPairFst ta _)
       | [(_, VPair va _)] <- args
       -> return $ XValue ta va
       | otherwise
       -> Nothing

      M.PrimPair    (M.PrimPairSnd _ tb)
       | [(_, VPair _ vb)] <- args
       -> return $ XValue tb vb
       | otherwise
       -> Nothing


    bool1 args f
      | [(_, VBool x)] <- args
      = return $ XValue BoolT (VBool (f x))
      | otherwise = Nothing

    bool2 args f
      | [(_, VBool x), (_, VBool y)] <- args
      = return $ XValue BoolT (VBool (f x y))
      | otherwise = Nothing

    int2 args f
      | [(_, VInt x), (_, VInt y)] <- args
      = return $ XValue BoolT (VBool (f x y))
      | otherwise = Nothing

    arith2 pp args f
      | [(_, VInt x), (_, VInt y)] <- args
      = let t = functionReturns (M.typeOfPrim pp)
            v = f x y
        in  return $ XValue t (VInt v)
      | otherwise = Nothing


takeValue :: Exp n p -> Maybe (ValType, BaseValue)
takeValue (XValue a b) = Just (a, b)
takeValue _            = Nothing

