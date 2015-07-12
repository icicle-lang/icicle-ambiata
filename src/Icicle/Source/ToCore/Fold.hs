-- | Convert queries into folds.
-- Used in body of group-bys, distincts,...

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Fold (
    convertFold
  , ConvertFoldResult(..)
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
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
import                  Data.List (zip)
import qualified        Data.Map    as Map


data ConvertFoldResult n
 = ConvertFoldResult
 { foldKons     :: C.Exp n
 , foldZero     :: C.Exp n
 , mapExtract   :: C.Exp n
 , typeFold     :: UniverseType
 , typeExtract  :: UniverseType
 } deriving (Eq, Ord, Show)


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
        => Query (a,UniverseType) n
        -> ConvertM a n (ConvertFoldResult n)
convertFold q
 = case contexts q of
    -- No contexts, just an expression
    []
     -- Nested query; recurse
     | Nested _ qq <- final q
     -> convertFold qq

     -- Primitive application
     | Just (p, (ann,retty), args) <- takePrimApps $ final q
     -> case p of
         -- Aggregates are relatively simple
         Agg SumA
          | [e] <- args
          -> do let retty' = baseTypeOrOption retty
                e' <- convertExp e

                n  <- lift fresh
                add <- convertPrim (Op Add) ann retty
                        [(CE.XVar n, retty)
                        ,(e', retty)]
                let k = CE.XLam n retty' add
                let z = someIfPossible retty $ CE.constI 0
                x    <- idFun retty'

                return $ ConvertFoldResult k z x retty retty
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

                return $ ConvertFoldResult k z x retty retty
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
                res <- mapM (convertFold . Query []) args

                let ts  = fmap typeFold         res
                let ts' = fmap baseTypeOrOption ts
                -- Create pairs for zeros
                (zz, tt) <- pairConstruct (fmap foldZero res) ts'

                -- For extraction:
                --  destruct the pairs,
                --  recursively extract the arguments,
                --  apply the primitive
                let cp ns
                        = convertPrim p ann retty
                            ((fmap (uncurry CE.XApp) (fmap mapExtract res `zip` ns)) `zip` fmap typeExtract res)
                xx       <- pairDestruct cp ts' (baseTypeOrOption retty)

                -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
                -- then box it up again in pairs.
                let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.XApp) (fmap foldKons res `zip` ns)) ts'
                kk       <- pairDestruct applyKs ts' tt

                let tt' = retty { baseType = tt }
                let needTraverse
                        | Just _ <- C.extractOption tt
                        = True
                        | otherwise
                        = False

                let tt''| Just t' <- C.extractOption tt
                        = possiblyUT $ retty { baseType = t' }
                        | otherwise
                        = definitelyUT $ retty { baseType = tt }

                kk'    <- if needTraverse
                          then traverseCompose tt' kk
                          else return kk
                let zz' = if needTraverse
                          then traverseIfPossible tt' zz
                          else zz
                let xx' = xx

                return
                 $ ConvertFoldResult kk' zz' xx' tt'' retty

     -- Variable lookup.
     -- The actual folding doesn't matter,
     -- we can just return const unit for the fold part,
     -- and at extract return the variable's value
     | Var (ann,retty) v <- final q
      -> do fs <- convertFeatures
            case Map.lookup v fs of
             Just (_, var')
              -> do i <- idFun (baseTypeOrOption retty)
                    n'v <- lift fresh
                    inp <- convertInputName
                    let k = CE.XLam n'v (baseTypeOrOption retty) $ var' $ CE.XVar inp
                    -- TODO argh why an int.
                    -- maybe this shouldn't be here,
                    -- and let of elem should actually be different.
                    -- yes - instead of this, Let where def is Elem should call convertExp instead of convertFold
                    return $ ConvertFoldResult k (CE.XValue (baseTypeOrOption retty) (VInt 13013)) i retty retty
             _
              -> do n'x <- lift fresh
                    v'  <- convertFreshenLookup ann v
                    let ut    = T.UnitT
                    let unit = CE.XValue ut VUnit

                    let k    = CE.XLam n'x ut $ unit
                    let z    = unit
                    let x    = CE.XLam n'x ut $ CE.XVar $ v'

                    return $ ConvertFoldResult k z x (definitelyUT $ snd $ annotOfExp $ final q) { baseType = ut } retty

     -- It must be a non-primitive application
     | otherwise
      -> convertError $ ConvertErrorExpApplicationOfNonPrimitive (fst $ annotOfExp $ final q) (final q)

    -- For filter, you convert the subquery as normal,
    -- then only apply the subquery's "k" when the filter predicate is true.
    --
    -- Note that this has different "history semantics" to the normal filter.
    (Filter _ e : _)
     -> do  res <- convertFold q'
            e'         <- convertExp  e
            prev       <- lift fresh
            let tt'     = baseType $ typeFold res
            let prev'   = CE.XVar prev
            let k' = CE.XLam prev tt'
                   ( CE.XPrim (C.PrimFold C.PrimFoldBool tt')
                     CE.@~ (foldKons res CE.@~ prev') CE.@~ prev' CE.@~ e' )
            return (res { foldKons = k' })

    (Windowed (ann,_) _ _ : _)
     -> errNotAllowed ann
    (Latest (ann,_) _ : _)
     -> errNotAllowed ann
    (GroupBy (ann,_) _ : _)
     -> errNotAllowed ann
    (Distinct (ann,_) _ : _)
     -> errNotAllowed ann


    (Let _ b def : _)
     -> do  resb <- convertFold (Query [] def)
            b' <- convertFreshenAdd b
            resq <- convertFold q'
            let tb'ret = baseType $ typeFold resb
            let tq'ret = baseType $ typeFold resq
            let u' = Universe { universeTemporality = universeTemporality $ universe $ typeFold resq
                              , universePossibility = maxOfPossibility (universePossibility $ universe $ typeFold resb) (universePossibility $ universe $ typeFold resq) }
            let t'     = UniverseType { universe = u', baseType = T.PairT tb'ret tq'ret}
            let pairOuter = baseTypeOrOption t'

            let tb' = baseTypeOrOption $ typeFold resb
            let tq' = baseTypeOrOption $ typeFold resq
            let pairNested = T.PairT tb' tq'
            let t'Nested = t' { baseType = pairNested }

            let mkPair x y
                   = CE.XPrim
                   (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair tb' tq')
                     CE.@~ x CE.@~ y
            let xproj which x
                   = CE.XPrim
                   (C.PrimMinimal $ Min.PrimPair $ which tb' tq')
                     CE.@~ x
            let xfst = xproj Min.PrimPairFst
            let xsnd = xproj Min.PrimPairSnd

            k' <- mapOptionLam pairOuter pairOuter
                $ \n' -> CE.XLet b' (foldKons resb CE.@~ someIfPossible (typeFold resb) (xfst n'))
                       $ traverseIfPossible t'Nested
                       $ mkPair (CE.XVar b') (foldKons resq CE.@~ (someIfPossible (typeFold resq) $ xsnd n'))

            let z' = CE.XLet b' (foldZero resb)
                   $ traverseIfPossible t'Nested
                   $ mkPair (CE.XVar b') (foldZero resq)

            x' <- mapOptionLam pairOuter (baseTypeOrOption $ typeExtract $ resq)
                $ \n' -> CE.XLet b' (mapExtract resb CE.@~ (someIfPossible (typeFold resb) $ xfst n'))
                                    (mapExtract resq CE.@~ (someIfPossible (typeFold resq) $ xsnd n'))

            return $ ConvertFoldResult k' z' x' t' (typeExtract resq)

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


