-- | Beta and let reduction for simple values
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Simp.Beta (
      beta
    , betaToLets
    , isSimpleValue
    ) where

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds

import P


-- | Beta and let reduction
beta :: (Ord n) => (Exp a n p -> Bool) -> Exp a n p -> Exp a n p
beta isValue toplevel
 = go toplevel
 where
  go xx
   = case xx of
      XApp _ p q
       | XLam _ n _ x <- go p
       , v <- go q
       , isValue v
       , Just x' <- substMaybe n v x
       -> go x'

      XApp a p q
       -> XApp a (go p) (go q)

      XLam a n t x
       -> XLam a n t $ go x

      XLet _ n v x
       | isValue v
       , Just x' <- substMaybe n v x
       -> go x'

      XLet a n v x
       -> XLet a n (go v) (go x)

      XValue{}        -> xx
      XVar{}          -> xx
      XPrim{}         -> xx

-- | Total beta: always convert (\n. f) x to  (let n = x in f)
betaToLets :: a -> Exp a n p -> Exp a n p
betaToLets a_let toplevel
 = go toplevel
 where
  go xx
   = case xx of
      XApp _ _ _
       | (f, x:xs)    <- takeApps xx
       , XLam _ n _ b <- f
       -> XLet a_let n (go x) (go $ makeApps a_let b xs)

      XApp a p q
       -> XApp a (go p) (go q)

      XLam a n t x
       -> XLam a n t $ go x

      XLet a n v x
       -> XLet a n (go v) (go x)

      XValue{}        -> xx
      XVar{}          -> xx
      XPrim{}         -> xx



-- | Check if expression is just a primitive or a variable.
isSimpleValue :: Exp a n p -> Bool
isSimpleValue xx
 = case xx of
    XPrim{} -> True
    XVar{}  -> True
    XValue{}-> True
    XLam{}  -> True
    _       -> False
