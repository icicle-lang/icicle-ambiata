{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Avalanche.Statement.Simp.Eval
     ( simpEvalX
     , simpEvalP
     ) where

import           Icicle.Common.Value
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.FixT

import           P

import           Data.Functor.Identity

simpEvalX   :: EvalPrim a n p
            -> (p -> Type)
            -> Exp a n p
            -> Exp a n p
simpEvalX ev ty xx
 = runIdentity $ once (simpEvalX' ev ty xx)


simpEvalX'  :: Monad m
            => EvalPrim a n p
            -> (p -> Type)
            -> Exp a n p
            -> FixT m (Exp a n p)
simpEvalX' ev ty = go
  where
    go xx = case xx of
      XApp a p q
       -> do p' <- go p
             q' <- go q
             let x' = XApp a p' q'
             case takePrimApps x' of
               Just (prim, as)
                 | Just args <- mapM takeValue as
                 -> case simpEvalP ev ty a prim args of
                     Just x''
                       -> progress x''
                     _ -> return x'
               _ -> return x'

      XLam a n t x1
        -> XLam a n t <$> go x1

      XLet a n p q
        -> XLet a n <$> go p <*> go q

      XVar{}   -> return xx
      XPrim{}  -> return xx
      XValue{} -> return xx


-- | Primitive Simplifier
--
simpEvalP   :: EvalPrim a n p
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

