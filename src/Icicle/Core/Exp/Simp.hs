{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Core.Exp.Simp
     ( simp
     , simpX
     , simpP
     ) where

import           Icicle.Common.Value
import           Icicle.Common.Exp              hiding (simp)
import           Icicle.Common.Exp.Simp.ANormal
import qualified Icicle.Common.Exp.Simp.Beta    as B
import           Icicle.Common.Fresh
import           Icicle.Common.Type
import qualified Icicle.Core.Exp                as C
import           Icicle.Core.Exp.Prim
import qualified Icicle.Core.Eval.Exp           as CE

import           P

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-- | Core Simplifier:
--   * a normal
--   * beta reduction
--   * constant folding for some primitives
--   * ...something exciting???
--
simp :: Ord n => a -> (C.Exp a n -> Bool) -> C.Exp a n -> Fresh n (C.Exp a n)
simp a_fresh isValue = anormal a_fresh . fixp (50 :: Int) (simpX a_fresh isValue)
 where
  fixp 0 f = f
  fixp n f = f . fixp (n-1) f


simpX :: Ord n => a -> (C.Exp a n -> Bool) -> C.Exp a n -> C.Exp a n
simpX a_fresh isValue = go
  where
    beta  = B.beta isValue
    go xx = case beta xx of
      -- * constant folding for some primitives
      XApp a p q
        | p' <- go p
        , q' <- go q
        , Just (prim, as) <- takePrimApps (XApp a p' q')
        , Just args       <- mapM (takeValue . go) as
        -> fromMaybe (XApp a p' q') (simpP a_fresh prim args)

      XApp a p q
        -> XApp a (go p) (go q)

      XLam a n t x1
        -> XLam a n t (go x1)

      XLet a n x1 x2
        | not $ n `Set.member` freevars (go x2)
        -> go x2
        | otherwise
        -> XLet a n (go x1) (go x2)

      b@(XVar{})   -> b
      b@(XPrim{})  -> b
      b@(XValue{}) -> b


-- | Primitive Simplifier
--
simpP :: Ord n => a -> Prim -> [Value a n Prim] -> Maybe (C.Exp a n)
simpP a_fresh p vs
 = case CE.evalPrim p vs of
    Right (VBase b)
     -> Just
      $ XValue a_fresh (functionReturns $ C.typeOfPrim p) b
    -- TODO: we could actually pull the
    -- heap out as let bindings, and so on..
    Right VFun{}
     -> Nothing
    Left _
     -> Nothing


takeValue :: Exp a n p -> Maybe (Value a n p)
takeValue (XValue _ _ b) = Just (VBase b)
-- We're pulling out a lambda as a closure.
-- However, we're ignoring the closure's heap.
-- This is fine - if the lambda references anything outside,
-- it will not evaluate and so won't be simplified.
takeValue (XLam _ n _ x) = Just (VFun Map.empty n x)
-- I promise this is exhaustive.
takeValue  _             = Nothing

