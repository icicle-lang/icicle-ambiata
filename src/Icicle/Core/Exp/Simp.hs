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
simp :: Ord n => (C.Exp n -> Bool) -> C.Exp n -> Fresh n (C.Exp n)
simp isValue = anormal . simpX isValue


simpX :: Ord n => (C.Exp n -> Bool) -> C.Exp n -> C.Exp n
simpX isValue = go
  where
    beta  = B.beta isValue
    go xx = case beta xx of
      -- * constant folding for some primitives
      XApp p q
        | p' <- go p
        , q' <- go q
        , Just (prim, as) <- takePrimApps (XApp p' q')
        , Just args    <- mapM (takeValue . go) as
        -> fromMaybe (XApp p' q') (simpP prim args)

      XApp p q
        -> XApp (go p) (go q)

      XLam n t x1
        -> XLam n t (go x1)

      XLet n x1 x2
        | not $ n `Set.member` freevars x2
        -> go x2
        | otherwise
        -> XLet n (go x1) (go x2)

      b@(XVar{})   -> b
      b@(XPrim{})  -> b
      b@(XValue{}) -> b


-- | Primitive Simplifier
--
simpP :: Ord n => Prim -> [Value n Prim] -> Maybe (C.Exp n)
simpP p vs
 = case CE.evalPrim p vs of
    Right (VBase b)
     -> Just
      $ XValue (functionReturns $ C.typeOfPrim p) b
    -- TODO: we could actually pull the
    -- heap out as let bindings, and so on..
    Right VFun{}
     -> Nothing
    Left _
     -> Nothing


takeValue :: Exp n p -> Maybe (Value n p)
takeValue (XValue _ b) = Just (VBase b)
-- We're pulling out a lambda as a closure.
-- However, we're ignoring the closure's heap.
-- This is fine - if the lambda references anything outside,
-- it will not evaluate and so won't be simplified.
takeValue (XLam n _ x) = Just (VFun Map.empty n x)
-- I promise this is exhaustive.
takeValue  _           = Nothing

