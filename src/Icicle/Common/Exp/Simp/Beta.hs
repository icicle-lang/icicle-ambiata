-- | Beta and let reduction for simple values
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Simp.Beta (
      beta
    , isSimpleValue
    ) where

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds

import P


-- | Beta and let reduction
beta :: (Show n, Show p, Ord n) => (Exp n p -> Bool) -> Exp n p -> Exp n p
beta isValue toplevel
 = go toplevel
 where
  go xx
   = case xx of
      XApp p q
       | XLam n _ x <- go p
       , v <- go q
       , isValue v
       , Just x' <- substMaybe n v x
       -> go x'
     
      XApp p q
       -> go p `XApp` go q

      XLam n t x
       -> XLam n t $ go x

      XLet n v x
       | isValue v
       , Just x' <- substMaybe n v x
       -> go x'

      XLet n v x
       -> XLet n (go v) (go x)
  
      XValue{}        -> xx
      XVar{}          -> xx
      XPrim{}         -> xx


-- | Check if expression is just a primitive or a variable.
isSimpleValue :: Exp n p -> Bool
isSimpleValue xx
 = case xx of
    XPrim{} -> True
    XVar{}  -> True
    XValue{}-> True
    _       -> False
