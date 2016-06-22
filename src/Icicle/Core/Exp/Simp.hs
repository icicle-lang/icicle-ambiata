{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Core.Exp.Simp
     ( simp
     , simpX
     , simpP
     , deadX
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

import qualified Data.Set                       as Set
import           Data.Hashable                  (Hashable)


-- | Core Simplifier:
--   * a normal
--   * beta reduction
--   * constant folding for some primitives
--   * ...something exciting???
--
simp :: (Hashable n, Eq n) => a -> (C.Exp a n -> Bool) -> C.Exp a n -> Fresh n (C.Exp a n)
simp a_fresh isValue = anormal a_fresh . deadX . fixp (simpX a_fresh isValue)
 where
  fixp f x
   | Just x' <- f x = fixp f x'
   | otherwise      = x


simpX :: (Hashable n, Eq n)=> a -> (C.Exp a n -> Bool) -> C.Exp a n -> Maybe (C.Exp a n)
simpX a_fresh isValue = go . beta
  where
    beta  = B.beta isValue
    -- * constant folding for some primitives
    go xx = case xx of
      XApp a p q
       -> do p' <- go p
             q' <- go q
             let x' = XApp a p' q'
             case takePrimApps x' of
               Just (prim, as)
                 | Just args <- mapM takeValue as
                 -> simpP a_fresh prim args
               _ -> return x'

      XLam a n t x1
        -> XLam a n t <$> go x1

      XLet a n p q
        | Just p' <- go p, Just q' <- go q -> Just $ XLet a n p' q'
        | Just p' <- go p, Nothing <- go q -> Just $ XLet a n p' q
        | Nothing <- go p, Just q' <- go q -> Just $ XLet a n p  q'
        | otherwise                        -> Nothing

      XVar{}   -> Nothing
      XPrim{}  -> Nothing
      XValue{} -> Nothing


-- | Primitive Simplifier
--
simpP :: (Hashable n, Eq n) => a -> Prim -> [Value a n Prim] -> Maybe (C.Exp a n)
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


-- | Dead binding removal
deadX :: (Hashable n, Eq n) => C.Exp a n -> C.Exp a n
deadX = fst . go
  where
    go xx = case xx of
      XApp a p q
        -> let (px, pf) = go p
               (qx, qf) = go q
           in  (XApp a px qx, Set.union pf qf)

      XLam a n t x1
        -> let (x1', fs) = go x1
           in  (XLam a n t x1', Set.delete n fs)

      XLet a n x1 x2
        -> let (x2', f2) = go x2
               (x1', f1) = go x1
           in  if   n `Set.member` f2
               then (XLet a n x1' x2', Set.union f1 f2)
               else (             x2',              f2)

      b@(XVar _ n) -> (b, Set.singleton n)
      b@(XPrim{})  -> (b, Set.empty)
      b@(XValue{}) -> (b, Set.empty)


