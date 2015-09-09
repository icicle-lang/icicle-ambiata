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
 { foldKons     :: C.Exp () n
 , foldZero     :: C.Exp () n
 , mapExtract   :: C.Exp () n
 , typeFold     :: T.ValType
 , typeExtract  :: T.ValType
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
        => Query (Annot a n) n
        -> ConvertM a n (ConvertFoldResult n)
convertFold q
 = case contexts q of
    -- No contexts, just an expression
    []
     -- Nested query; recurse
     | Nested _ qq <- final q
     -> convertFold qq

     -- Primitive application
     | Just (p, Annot { annAnnot = ann, annResult = retty }, args) <- takePrimApps $ final q
     -- Non-aggregate primitive operations such as (+) or (/) are a bit more involved:
     -- we convert the arguments to folds,
     -- then store the accumulator as nested pairs of arguments
     -- then, for the extract we destruct the pairs and apply the operator normally.
      -> do -- Convert all arguments
            -- (create a query out of the expression,
            --  just because there is no separate convertFoldX function)
            res <- mapM (convertFold . Query []) args
            retty' <- convertValType' retty

            let ts  = fmap typeFold         res
            -- Create pairs for zeros
            (zz, tt) <- pairConstruct (fmap foldZero res) ts

            -- For extraction:
            --  destruct the pairs,
            --  recursively extract the arguments,
            --  apply the primitive
            let cp ns
                    = convertPrim p ann retty
                        ((fmap (uncurry CE.xApp) (fmap mapExtract res `zip` ns)) `zip` fmap (annResult . annotOfExp) args)
            xx       <- pairDestruct cp ts retty

            -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
            -- then box it up again in pairs.
            let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.xApp) (fmap foldKons res `zip` ns)) ts
            kk       <- pairDestruct applyKs ts tt

            return $ ConvertFoldResult kk zz xx tt retty'

     -- Variable lookup.
     | Var (Annot { annAnnot = ann, annResult = retty }) v <- final q
      -> do bound <- convertFreshenLookupMaybe v
            fs <- convertFeatures
            -- Check if it is a scalar variable or postcomputation
            case bound of
             Just v'
              | TemporalityPure  <- getTemporalityOrPure retty
              -> do n'ignore <- lift fresh
                    retty' <- convertValType' retty
                    let k = CE.xLam n'ignore retty' $ CE.xVar v'
                    return $ ConvertFoldResult k (CE.xVar v') k retty' retty'

              | TemporalityElement <- getTemporalityOrPure retty
              -> do retty' <- convertValType' retty
                    i <- idFun retty'
                    n'v <- lift fresh
                    let k = CE.xLam n'v retty' $ CE.xVar v'
                    let err = CE.xValue retty' $ VException ExceptScalarVariableNotAvailable
                    return $ ConvertFoldResult k err i retty' retty'

             -- For aggregate variables, the actual folding doesn't matter:
             -- we can just return const unit for the fold part,
             -- and at extract return the variable's value
              | otherwise
              -> do n'x <- lift fresh
                    retty' <- convertValType' retty
                    let ut    = T.UnitT
                    let unit = CE.xValue ut VUnit

                    let k    = CE.xLam n'x ut $ unit
                    let z    = unit
                    let x    = CE.xLam n'x ut $ CE.xVar $ v'

                    return $ ConvertFoldResult k z x ut retty'


             -- Otherwise it must be a feature
             _
              | Just (_, var') <- Map.lookup v fs
              -> do -- Creating a fold from a scalar variable is strange, since
                    -- the scalar variable is only available inside each iteration.
                    -- For the konstrukt, we just return the current value.
                    -- For zero, we have no value yet - we must throw an exception.
                    -- This is bad, but is not a problem in practice since scalar variables
                    -- can only be used inside each iteration.
                    -- The exception value won't end up being used.
                    --
                    -- Extract, after the fold is finished, is just identity.
                    -- Const Unit would work too, since the extracted value should
                    -- never be used for the same reason the zero is not used.

                    retty' <- convertValType' retty
                    i <- idFun retty'
                    n'v <- lift fresh
                    inp <- convertInputName
                    let k = CE.xLam n'v retty' $ var' $ CE.xVar inp
                    let err = CE.xValue retty' $ VException ExceptScalarVariableNotAvailable
                    return $ ConvertFoldResult k err i retty' retty'

              | otherwise
              -> convertError $ ConvertErrorExpNoSuchVariable ann v

     | Case (Annot { annAnnot = ann, annResult = retty }) scrut pats <- final q
      -> do -- Case expressions are very similar to primops.
            -- We know that the scrutinee and the patterns are all aggregates.
            -- Elements are handled elsewhere.
            let args = scrut : fmap snd pats
            res <- mapM (convertFold . Query []) args
            retty' <- convertValType' retty
            scrutT <- convertValType' $ annResult $ annotOfExp scrut

            let ts  = fmap (typeFold) res
            -- Create pairs for zeros
            (zz, tt) <- pairConstruct (fmap foldZero res) ts

            -- For extraction:
            --  reconstruct the case
            --  and use convertExp
            let cp ns
                    = case fmap (uncurry CE.xApp) (fmap mapExtract res `zip` ns) of
                        (s:alts)
                         -> convertCase (final q) s (fmap fst pats `zip` alts) scrutT retty'
                        _
                         -> convertError $ ConvertErrorBadCaseNoDefault ann (final q)
            xx       <- pairDestruct cp ts retty

            -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
            -- then box it up again in pairs.
            let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.xApp) (fmap foldKons res `zip` ns)) ts
            kk       <- pairDestruct applyKs ts tt

            return $ ConvertFoldResult kk zz xx tt retty'


     -- It must be a non-primitive application
     | otherwise
      -> convertError $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot $ annotOfExp $ final q) (final q)

    -- For filter, you convert the subquery as normal,
    -- then only apply the subquery's "k" when the filter predicate is true.
    --
    -- Note that this has different "history semantics" to the normal filter.
    (Filter _ e : _)
     -> do  res <- convertFold q'
            e'         <- convertExp  e
            prev       <- lift fresh
            let tt'    = typeFold res
            let prev'  = CE.xVar prev
            let k' = CE.xLam prev tt'
                   ( CE.xPrim (C.PrimFold C.PrimFoldBool tt')
                     CE.@~ (foldKons res CE.@~ prev') CE.@~ prev' CE.@~ e' )
            return (res { foldKons = k' })

    (Windowed (Annot { annAnnot = ann }) _ _ : _)
     -> errNotAllowed ann
    (Latest (Annot { annAnnot = ann }) _ : _)
     -> errNotAllowed ann
    (GroupBy (Annot { annAnnot = ann }) _ : _)
     -> errNotAllowed ann
    (GroupFold (Annot { annAnnot = ann }) _ _ _ : _)
     -> errNotAllowed ann
    (Distinct (Annot { annAnnot = ann }) _ : _)
     -> errNotAllowed ann


    (Let _ b def : _)
     | TemporalityPure  <- getTemporalityOrPure $ annResult $ annotOfExp def
     -> do  def' <- convertExp def
            n'   <- lift fresh
            t' <- convertValType' $ annResult $ annotOfExp def
            let res = ConvertFoldResult (CE.xLam n' t' def') def' (CE.xLam n' t' def') t' t'
            convertAsLet b res

     | TemporalityElement  <- getTemporalityOrPure $ annResult $ annotOfExp def
     -> do  def' <- convertExp def
            n'   <- lift fresh
            t' <- convertValType' $ annResult $ annotOfExp def
            let err = CE.xValue t' $ VException ExceptScalarVariableNotAvailable
            let res = ConvertFoldResult (CE.xLam n' t' def') err (CE.xLam n' t' $ CE.xVar n') t' t'
            convertAsLet b res

     | otherwise
     -> do  resb <- convertFold (Query [] def)
            convertAsLet b resb

    (LetFold _ f@Fold{ foldType = FoldTypeFoldl1 } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f
            let tO = T.OptionT tU

            -- Generate fresh names
            -- Current accumulator
            -- : Option tU
            n'a     <- lift fresh

            z   <- convertExp (foldInit f)
            -- Current accumulator is only available in worker
            n'a' <- convertFreshenAdd $ foldBind f
            k   <- convertExp (foldWork f)

            let opt r = CE.xPrim $ C.PrimFold (C.PrimFoldOption tU) r
            -- Wrap zero and kons up in Some
            let k' = CE.xLam n'a tO
                   ( opt tO
                     CE.@~ CE.xLam n'a' tU (CE.some tU $ k)
                     CE.@~ CE.some tU z
                     CE.@~ CE.xVar n'a)

            let x' = CE.xLam n'a tO
                   ( opt tU
                     CE.@~ CE.xLam   n'a' tU (CE.xVar n'a')
                     CE.@~ CE.xValue tU (VException ExceptFold1NoValue)
                     CE.@~ CE.xVar   n'a )

            let res = ConvertFoldResult k' (CE.xValue tO VNone) x' tO tU
            convertAsLet (foldBind f) res


    (LetFold _ f@Fold{ foldType = FoldTypeFoldl } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f


            z   <- convertExp (foldInit f)
            -- Current accumulator is only available in worker
            n'a <- convertFreshenAdd $ foldBind f
            k   <- convertExp (foldWork f)

            let k' = CE.xLam n'a tU k

            x' <- idFun tU

            let res = ConvertFoldResult k' z x' tU tU
            convertAsLet (foldBind f) res



 where
  q' = q { contexts = drop 1 $ contexts q }

  errNotAllowed ann
   = convertError $ ConvertErrorContextNotAllowedInGroupBy ann q

  -- Construct an identity function
  idFun tt = lift fresh >>= \n -> return (CE.xLam n tt (CE.xVar n))

  -- Create nested pair type for storing the result of subexpressions
  pairTypes ts
   = foldr T.PairT T.UnitT ts

  -- Create nested pairs of arguments
  pairConstruct xs ts
   = return
   $ foldr
   (\(xa,ta) (x',t')
    -> ( CE.xPrim
            (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta t')
            CE.@~ xa CE.@~ x'
       , T.PairT ta t'))
   ( CE.xValue T.UnitT VUnit, T.UnitT )
   ( zip xs ts )

  -- Destruct nested pairs.
  -- Call "f" with expression for each element of the pair.
  pairDestruct f [] _ret
   = do nl <- lift fresh
        f' <- f []
        return $ CE.xLam nl T.UnitT $ f'

  pairDestruct f (t1:ts) ret
   = do nl <- lift fresh
        n1 <- lift fresh

        let f' xs = f (CE.xVar n1 : xs)
        let tr    = pairTypes ts

        rest <- pairDestruct f' ts ret

        let xfst = CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t1 tr) CE.@~ CE.xVar nl
        let xsnd = CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t1 tr) CE.@~ CE.xVar nl

        let xx = CE.xLam nl (T.PairT t1 tr)
               $ CE.xLet n1 xfst
               ( rest CE.@~ xsnd )

        return xx


  convertAsLet b resb
   =    do  b'     <- convertFreshenAdd b
            resq   <- convertFold q'
            let tb' = typeFold resb
            let tq' = typeFold resq
            let tpair = T.PairT tb' tq'

            let mkPair x y
                   = CE.xPrim
                   (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair tb' tq')
                     CE.@~ x CE.@~ y
            let xproj which x
                   = CE.xPrim
                   (C.PrimMinimal $ Min.PrimPair $ which tb' tq')
                     CE.@~ x
            let xfst = xproj Min.PrimPairFst
            let xsnd = xproj Min.PrimPairSnd

            n' <- lift fresh
            let k'  = CE.xLam n' tpair
                    $ CE.xLet b' (foldKons resb CE.@~ xfst (CE.xVar n'))
                    $ mkPair (CE.xVar b') (foldKons resq CE.@~ xsnd (CE.xVar n'))

            let z'  = CE.xLet b' (foldZero resb)
                    $ mkPair (CE.xVar b') (foldZero resq)

            let x'  = CE.xLam n' tpair
                    $ CE.xLet b' (mapExtract resb CE.@~ xfst (CE.xVar n'))
                                 (mapExtract resq CE.@~ xsnd (CE.xVar n'))

            return $ ConvertFoldResult k' z' x' tpair (typeExtract resq)

  convertValType' = convertValType (annAnnot $ annotOfQuery q)

