-- | Convert queries into folds.
-- Used in body of group-bys, distincts,...

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Fold (
    convertFold
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.ToCore.Exp
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base


import                  P

import                  Control.Monad.Trans.Class
import                  Data.List (zip, unzip4)



-- | Convert the body of a group by (or other query) into a fold:
--
-- The fold is described by:
--  Konstrukt : a -> b -> a
--  Zero      : a
--  Xtract    : a -> c
--
-- The extract is used for any postprocessing that can only be done on the
-- final result of the accumulator.
--
-- So for example, a sum would be
--  K = (+)
--  Z = 0
--  X = id
--
-- but if we wanted mean, we would store a pair of values in the accumulator,
-- the sum and the count, and then the extract would divide the two:
--  K = (\(s,c) v   -> (s + v, c + 1))
--  Z =                (0, 0)
--  X = (\(s,c)     -> s / c)
-- 
-- 
-- Not all subqueries are supported: windowing, grouping and distincts are banned.
--
--
convertFold
        :: Ord n
        => FeatureContext n
        -> Query (a,UniverseType) n
        -> ConvertM a n (C.Exp n, C.Exp n, C.Exp n, UniverseType)
convertFold fs q
 = case contexts q of
    -- No contexts, just an expression
    []
     -- Nested query; recurse
     | Nested _ qq <- final q
     -> convertFold fs qq

     -- Primitive application
     | Just (p, (ann,retty), args) <- takePrimApps $ final q
     -> case p of
         -- Aggregates are relatively simple
         Agg SumA
          | [e] <- args
          -> do let retty' = baseType retty
                e' <- convertExp fs e

                n  <- lift fresh
                let k = CE.XLam n retty'
                      ( CE.XVar n CE.+~ e' )
                let z = CE.constI 0
                x    <- idFun retty'

                return (k, z, x, retty)
          | otherwise
          -> errAggBadArgs

         Agg Count
          | [] <- args
          -> do let retty' = baseType retty

                n  <- lift fresh
                let k = CE.XLam n retty'
                      ( CE.XVar n CE.+~ CE.constI 1 )
                let z = CE.constI 0
                x    <- idFun retty'

                return (k, z, x, retty)
          | otherwise
          -> errAggBadArgs


         -- Non-aggregate primitive operations such as (+) or (/) are a bit more involved:
         -- we convert the arguments to folds,
         -- then store the accumulator as nested pairs of arguments
         -- then, for the extract we destruct the pairs and apply the operator normally.
         _
          -> do -- Convert all arguments
                -- (create a query out of the expression,
                --  just because there is no separate convertFoldX function)
                (ks, zs, xs, ts) <- unzip4 <$> mapM (convertFold fs . Query []) args

                let ts' = fmap baseType ts
                -- Create pairs for zeros
                (zz, tt) <- pairConstruct zs ts'

                -- For extraction:
                --  destruct the pairs,
                --  recursively extract the arguments,
                --  apply the primitive
                let cp ns
                        = convertPrim p ann retty
                            ((fmap (uncurry CE.XApp) (xs `zip` ns)) `zip` ts)
                xx       <- pairDestruct cp ts' (baseType retty)

                -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
                -- then box it up again in pairs.
                let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.XApp) (ks `zip` ns)) ts'
                kk       <- pairDestruct applyKs ts' tt

                return (kk, zz, xx, retty { baseType = tt })

     -- Variable lookup.
     -- The actual folding doesn't matter,
     -- we can just return const unit for the fold part,
     -- and at extract return the variable's value
     | Var _ v <- final q
      -> do n'x <- lift fresh
            let ut    = T.UnitT
            let unit = CE.XValue ut VUnit
            
            let k    = CE.XLam n'x ut $ unit
            let z    = unit
            let x    = CE.XLam n'x ut $ CE.XVar $ Name $ v
            
            return (k, z, x, (snd $ annotOfExp $ final q) { baseType = ut })

     -- It must be a non-primitive application
     | otherwise
      -> convertError $ ConvertErrorExpApplicationOfNonPrimitive (fst $ annotOfExp $ final q) (final q)

    -- For filter, you convert the subquery as normal,
    -- then only apply the subquery's "k" when the filter predicate is true.
    --
    -- Note that this has different "history semantics" to the normal filter.
    (Filter _ e : _)
     -> do  (k,z,x,tt) <- convertFold fs q'
            e'         <- convertExp  fs e
            prev       <- lift fresh
            let tt'     = baseType tt
            let prev'   = CE.XVar prev
            let k' = CE.XLam prev tt'
                   ( CE.XPrim (C.PrimFold C.PrimFoldBool tt')
                     CE.@~ (k CE.@~ prev') CE.@~ prev' CE.@~ e' )
            return (k', z, x, tt)

    (Windowed (ann,_) _ _ : _)
     -> errNotAllowed ann
    (Latest (ann,_) _ : _)
     -> errNotAllowed ann
    (GroupBy (ann,_) _ : _)
     -> errNotAllowed ann
    (Distinct (ann,_) _ : _)
     -> errNotAllowed ann
    -- TODO: let and letfold should probably be allowed
    (Let (ann,_) _ _ : _)
     -> errNotAllowed ann
    (LetFold (ann,_) _ : _)
     -> errNotAllowed ann


 where
  q' = q { contexts = drop 1 $ contexts q }

  errNotAllowed ann
   = convertError $ ConvertErrorContextNotAllowedInGroupBy ann q

  errAggBadArgs
   = convertError
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp $ final q) (final q)


  -- Construct an identity function
  idFun tt = lift fresh >>= \n -> return (CE.XLam n tt (CE.XVar n))

  -- Create nested pair type for storing the result of subexpressions
  pairTypes ts
   = foldr T.PairT T.UnitT ts

  -- Create nested pairs of arguments
  pairConstruct xs ts
   = return
   $ foldr
   (\(xa,ta) (x',t')
    -> ( CE.XPrim
            (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta t')
            CE.@~ xa CE.@~ x'
       , T.PairT ta t'))
   ( CE.XValue T.UnitT VUnit, T.UnitT )
   ( zip xs ts )

  -- Destruct nested pairs.
  -- Call "f" with expression for each element of the pair.
  pairDestruct f [] _ret
   = do nl <- lift fresh
        f' <- f []
        return $ CE.XLam nl T.UnitT $ f'

  pairDestruct f (t1:ts) ret
   = do nl <- lift fresh
        n1 <- lift fresh

        let f' xs = f (CE.XVar n1 : xs)
        let tr    = pairTypes ts

        rest <- pairDestruct f' ts ret

        let xfst = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t1 tr) CE.@~ CE.XVar nl
        let xsnd = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t1 tr) CE.@~ CE.XVar nl

        let xx = CE.XLam nl (T.PairT t1 tr)
               $ CE.XLet n1 xfst
               ( rest CE.@~ xsnd )

        return xx


