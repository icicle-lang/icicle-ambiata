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
 , typeFold     :: UniverseType n
 , typeExtract  :: UniverseType n
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
        => Query (a,UniverseType n) n
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
                let ts' = fmap baseType ts
                -- Create pairs for zeros
                (zz, tt) <- pairConstruct (fmap foldZero res) ts'

                -- For extraction:
                --  destruct the pairs,
                --  recursively extract the arguments,
                --  apply the primitive
                let cp ns
                        = convertPrim p ann
                            ((fmap (uncurry CE.XApp) (fmap mapExtract res `zip` ns)) `zip` fmap typeExtract res)
                xx       <- pairDestruct cp ts'

                -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
                -- then box it up again in pairs.
                let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.XApp) (fmap foldKons res `zip` ns)) ts'
                kk       <- pairDestruct applyKs ts'

                let tt' = retty { baseType = tt }
                return $ ConvertFoldResult kk zz xx tt' retty

     -- Variable lookup.
     | Var (ann,retty) v <- final q
      -> do fs <- convertFeatures
            -- Check if it is a scalar variable or postcomputation
            case Map.lookup v fs of
             Just (_, var')
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
                    t'  <- convertValType (fst $ annotOfQuery q) $ baseType retty
                    i <- idFun t'
                    n'v <- lift fresh
                    inp <- convertInputName
                    let k = CE.XLam n'v t' $ var' $ CE.XVar inp
                    let err = CE.XValue t' $ VException ExceptScalarVariableNotAvailable
                    return $ ConvertFoldResult k err i retty retty

             _
              | Pure <- universeTemporality $ universe retty
              -> do v'  <- convertFreshenLookup ann v
                    n'ignore <- lift fresh
                    t'  <- convertValType (fst $ annotOfQuery q) $ baseType retty
                    let k = CE.XLam n'ignore t' $ CE.XVar v'
                    return $ ConvertFoldResult k (CE.XVar v') k retty retty

              | Elem <- universeTemporality $ universe retty
              -> do v'  <- convertFreshenLookup ann v
                    t'  <- convertValType (fst $ annotOfQuery q) $ baseType retty
                    i <- idFun t'
                    n'v <- lift fresh
                    let k = CE.XLam n'v t' $ CE.XVar v'
                    let err = CE.XValue t' $ VException ExceptScalarVariableNotAvailable
                    return $ ConvertFoldResult k err i retty retty

             -- For aggregate variables, the actual folding doesn't matter:
             -- we can just return const unit for the fold part,
             -- and at extract return the variable's value
              | otherwise
              -> do n'x <- lift fresh
                    v'  <- convertFreshenLookup ann v
                    let ut    = T.ValType T.UnitT
                    let unit = CE.XValue ut VUnit

                    let k    = CE.XLam n'x ut $ unit
                    let z    = unit
                    let x    = CE.XLam n'x ut $ CE.XVar $ v'

                    return $ ConvertFoldResult k z x (definitelyUT $ snd $ annotOfExp $ final q) { baseType = BaseType T.UnitT } retty

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
            tt'        <- convertValType (fst $ annotOfExp e) $ baseType $ typeFold res
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
     | Pure <- universeTemporality $ universe $ snd $ annotOfExp def
     -> do  def' <- convertExp def
            n'   <- lift fresh
            let t  = snd $ annotOfExp def
            let t' = baseType t
            t'' <- convertValType (fst $ annotOfExp def) t'
            let res = ConvertFoldResult (CE.XLam n' t'' def') def' (CE.XLam n' t'' def') t t
            convertAsLet b res

     | otherwise
     -> do  resb <- convertFold (Query [] def)
            convertAsLet b resb

    (LetFold _ f@Fold{ foldType = FoldTypeFoldl1 } : _)
     -> do  -- Type helpers
            let tU = baseType $ snd $ annotOfExp $ foldWork f
            tU' <- convertValType (fst $ annotOfExp $ foldWork f) tU
            let tO = BaseType $ T.OptionT tU
            tO' <- convertValType (fst $ annotOfExp $ foldWork f) tO

            -- Generate fresh names
            -- Current accumulator
            -- : Option tU
            n'a     <- lift fresh
            -- Fully unwrapped accumulator
            -- :        tU
            n'a' <- convertFreshenAdd $ foldBind f


            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            z   <- convertExp (foldInit f)
            k   <- convertExp (foldWork f)

            let opt r = CE.XPrim $ C.PrimFold (C.PrimFoldOption tU') r
            -- Wrap zero and kons up in Some
            let k' = CE.XLam n'a tO'
                   ( opt tO'
                     CE.@~ CE.XLam n'a' tU' (CE.some tU' $ k)
                     CE.@~ CE.some tU' z
                     CE.@~ CE.XVar n'a)

            let x' = CE.XLam n'a tO'
                   ( opt tU'
                     CE.@~ CE.XLam n'a' tU' (CE.XVar n'a')
                     CE.@~ CE.XValue tU' (VException ExceptFold1NoValue)
                     CE.@~ CE.XVar n'a )

            let t' = snd $ annotOfExp $ foldWork f

            let res = ConvertFoldResult k' (CE.XValue tO' VNone) x' (t' { baseType = tO } ) t'
            convertAsLet (foldBind f) res


    (LetFold _ f@Fold{ foldType = FoldTypeFoldl } : _)
     -> do  -- Type helpers
            tU <- convertValType (fst $ annotOfExp $ foldWork f) $ baseType $ snd $ annotOfExp $ foldWork f

            -- Generate fresh names
            -- Current accumulator
            n'a <- convertFreshenAdd $ foldBind f

            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            z   <- convertExp (foldInit f)
            k   <- convertExp (foldWork f)

            let k' = CE.XLam n'a tU k

            x' <- idFun tU

            let t' = snd $ annotOfExp $ foldWork f
            let res = ConvertFoldResult k' z x' t' t'
            convertAsLet (foldBind f) res



 where
  q' = q { contexts = drop 1 $ contexts q }

  errNotAllowed ann
   = convertError $ ConvertErrorContextNotAllowedInGroupBy ann q

  -- Construct an identity function
  idFun tt = lift fresh >>= \n -> return (CE.XLam n tt (CE.XVar n))

  -- Create nested pair type for storing the result of subexpressions
  pairTypes ts
   = foldr (\a b -> T.ValType $ T.PairT a b) (T.ValType T.UnitT)
   <$> mapM (convertValType $ fst $ annotOfQuery q) ts

  -- Create nested pairs of arguments
  pairConstruct xs ts
   = foldM pairConstruct'
   ( CE.XValue (T.ValType T.UnitT) VUnit, BaseType T.UnitT )
   ( zip xs ts )

  pairConstruct' (xa,ta) (x',t')
   = do ta' <- convertValType (fst $ annotOfQuery q) ta
        t'' <- convertValType (fst $ annotOfQuery q) t'
        return ( CE.XPrim
                    (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta' t'')
                    CE.@~ xa CE.@~ x'
               , BaseType $ T.PairT ta t')

  -- Destruct nested pairs.
  -- Call "f" with expression for each element of the pair.
  pairDestruct f []
   = do nl <- lift fresh
        f' <- f []
        return $ CE.XLam nl (T.ValType T.UnitT) $ f'

  pairDestruct f (t1:ts)
   = do nl <- lift fresh
        n1 <- lift fresh

        let f' xs = f (CE.XVar n1 : xs)
        t1' <- convertValType (fst $ annotOfQuery q) t1
        tr <- pairTypes ts

        rest <- pairDestruct f' ts

        let xfst = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t1' tr) CE.@~ CE.XVar nl
        let xsnd = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t1' tr) CE.@~ CE.XVar nl

        let xx = CE.XLam nl (T.ValType $ T.PairT t1' tr)
               $ CE.XLet n1 xfst
               ( rest CE.@~ xsnd )

        return xx


  convertAsLet b resb
   =    do  b' <- convertFreshenAdd b
            resq <- convertFold q'
            let tb'ret = baseType $ typeFold resb
            let tq'ret = baseType $ typeFold resq
            let u' = Universe { universeTemporality = universeTemporality $ universe $ typeFold resq
                              , universePossibility = maxOfPossibility (universePossibility $ universe $ typeFold resb) (universePossibility $ universe $ typeFold resq) }
            let t'     = UniverseType { universe = u', baseType = BaseType $ T.PairT tb'ret tq'ret}
            pairOuter <- convertValType (fst $ annotOfQuery q) $ baseType t'

            tb' <- convertValType (fst $ annotOfQuery q) $ baseType $ typeFold resb
            tq' <- convertValType (fst $ annotOfQuery q) $ baseType $ typeFold resq

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

            n' <- lift fresh
            let k'  = CE.XLam n' pairOuter
                    $ CE.XLet b' (foldKons resb CE.@~ xfst (CE.XVar n'))
                    $ mkPair (CE.XVar b') (foldKons resq CE.@~ xsnd (CE.XVar n'))

            let z' = CE.XLet b' (foldZero resb)
                   $ mkPair (CE.XVar b') (foldZero resq)

            let x' = CE.XLam n' pairOuter
                    $ CE.XLet b' (mapExtract resb CE.@~ xfst (CE.XVar n'))
                                 (mapExtract resq CE.@~ xsnd (CE.XVar n'))

            return $ ConvertFoldResult k' z' x' t' (typeExtract resq)


