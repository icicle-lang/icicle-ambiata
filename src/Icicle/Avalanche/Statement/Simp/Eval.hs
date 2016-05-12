{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Avalanche.Statement.Simp.Eval
     ( simpEvalX
     , simpEvalP
     ) where

import           Icicle.Common.Value
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           P

import           Data.Hashable (Hashable)

simpEvalX   :: (Hashable n, Eq n)
            => EvalPrim a n p
            -> (p -> Type)
            -> Exp a n p
            -> Exp a n p
simpEvalX ev ty = fixp (simpEvalX' ev ty)
 where
  fixp f x
   | Just x' <- f x = fixp f x'
   | otherwise      = x


simpEvalX'  :: (Hashable n, Eq n)
            => EvalPrim a n p
            -> (p -> Type)
            -> Exp a n p
            -> Maybe (Exp a n p)
simpEvalX' ev ty = go
  where
    go xx = case xx of
      XApp a p q
        | mp <- go p
        , mq <- go q
        , p' <- fromMaybe p mp
        , q' <- fromMaybe q mq
        , x' <- XApp a p' q'
        -> case takePrimApps x' of
             Just (prim, as)
               | Just args <- mapM takeValue as
               -> case simpEvalP ev ty a prim args of
                   Just x''
                     -> return x''
                   _ -> XApp a <$> mp <*> mq
             _ -> XApp a <$> mp <*> mq

        | Just p' <- go p, Just q' <- go q -> Just $ XApp a p' q'
        | Just p' <- go p, Nothing <- go q -> Just $ XApp a p' q
        | Nothing <- go p, Just q' <- go q -> Just $ XApp a p  q'
        | otherwise                        -> Nothing

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
simpEvalP   :: (Hashable n, Eq n)
            => EvalPrim a n p
            -> (p -> Type)
            -> a
            -> p
            -> [Value a n p]
            -> Maybe (Exp a n p)
simpEvalP ev ty a_fresh p vs
 -- TODO: evaluation function should not return value for underapplied primitives
 -- uncomment if you dare
 | length (functionArguments $ ty p) == length vs
 = case ev p vs of
    Right (VBase b)
     -> Just
      $ XValue a_fresh (functionReturns $ ty p) b
    -- TODO: we could actually pull the
    -- heap out as let bindings, and so on..
    Right VFun{}
     -> Nothing
    Left _
     -> Nothing
 | otherwise
 = Nothing

