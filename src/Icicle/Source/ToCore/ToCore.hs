{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.ToCore (
    convertQueryTop
  , convertQuery
--  , convertArray
--  , convertReduce
--  , convertGroupBy
--  , convertExp
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

-- TODO: this should not really rely on a specific name type,
-- after structs are supported.
-- But for now just look for variable called "value" and know that is the
-- input element.
import                  Icicle.Source.Lexer.Token (Variable(..))

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Exp.Simp.Beta    as Beta
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base


import                  P

import                  Control.Monad.Trans.Class
import                  Data.List (zip, unzip, unzip4)


type Nm = Name Variable

convertQueryTop
        :: QueryTop (a,UniverseType) Variable
        -> ConvertM a Variable (C.Program Variable)
convertQueryTop qt
 = do   inp <- fresh
        -- TODO: look this up in context
        let inpTy = T.PairT T.IntT T.DateTimeT

        (bs,ret) <- convertQuery inp inpTy (query qt)
        let bs'   = strm inp C.Source <> bs
        return (programOfBinds inpTy bs' ret)


convertQuery
        :: Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (CoreBinds Variable, Nm)
convertQuery n nt q
 = case contexts q of
    []
     -> convertReduce n nt (final q)

    -- TODO: need to convert "filter blah ~> windowed blah" into "windowed blah ~> filter blah"
    (Windowed _ (Days days) Nothing : _)
     -> do  n'      <- fresh
            (bs, b) <- convertQuery n' nt q'
            let bs'  = strm n' (C.SourceWindowedDays days) <> bs
            return (bs', b)
    -- TODO: support other ranges
    (Windowed (ann,_) _ _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "support window ranges"

    (Latest (ann,_) i : _)
     -> do  n'      <- fresh
            (bs, b) <- convertArray ann n' nt q'
            let bs'  = red n' (C.RLatest nt (CE.constI i) n) <> bs
            return (bs', b)

    (GroupBy (ann,retty) e : _)
     -> do  (t1,t2) <- getGroupByMapType ann retty
            n'      <- fresh
            n''     <- fresh
            nmap    <- fresh
            nval    <- fresh
     
            (k,z,x,tV)
                    <- convertGroupBy     nval nt q'
            e'      <- convertExp         nval nt e

            let beta = Beta.betaToLets
                     . Beta.beta Beta.isSimpleValue
            let k'   = beta k
            let z'   = beta z
            let x'   = beta x

            let mapt = T.MapT t1 tV

            let insertOrUpdate
                  = CE.XLam nmap mapt
                  $ CE.XLam nval nt
                  ( CE.XPrim (C.PrimMap $ C.PrimMapInsertOrUpdate t1 tV)
                    CE.@~  k'
                    CE.@~ (k' CE.@~ z')
                    CE.@~  e'
                    CE.@~ CE.XVar nmap )

            let r = red n' 
                  $ C.RFold nt mapt insertOrUpdate 
                  ( CE.emptyMap t1 tV)
                    n
            let p = post n''
                  ( CE.XPrim
                        (C.PrimMap $ C.PrimMapMapValues t1 tV t2)
                    CE.@~ x' CE.@~ CE.XVar n' )

            return (r <> p, n'')

    (Filter _ e : _)
     -> do  n'      <- fresh
            nv      <- fresh
            e'      <- convertExp nv nt e

            (bs, b) <- convertQuery n' nt q'
            let bs'  = strm n' (C.STrans (C.SFilter nt) e' n) <> bs

            return (bs', b)


    -- TODO: distinct, let, let fold
    (Distinct (ann,_) _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "convertQuery.Distinct"
    (Let (ann,_) _ _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "convertQuery.Let"
    (LetFold (ann,_) _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "convertQuery.LetFold"

        
 where
  q' = q { contexts = drop 1 $ contexts q }

  getGroupByMapType ann ty
   | UniverseType (Group t1) t2         <- ty
   = return (t1, t2)
   | UniverseType AggU (T.MapT t1 t2)   <- ty
   = return (t1, t2)
   | otherwise
   = lift $ Left $ ConvertErrorGroupByHasNonGroupResult ann ty


convertReduce
        :: Nm   -> T.ValType
        -> Exp (a,UniverseType) Variable
        -> ConvertM a Variable (CoreBinds Variable, Nm)
convertReduce n t xx
 | Just (p, (_,ty), args) <- takePrimApps xx
 = case p of
    Agg Count
     | [] <- args
     -> fresh >>= mkFold T.IntT (\na _nv -> na CE.+~ CE.constI 1) (CE.constI 0)
     | otherwise
     -> errAggBadArgs

    Agg SumA
     | [x] <- args
     -> do  nv <- fresh
            x' <- convertExp nv t x
            mkFold T.IntT (\na _nv -> na CE.+~ x') (CE.constI 0) nv
     | otherwise
     -> errAggBadArgs


    --(Agg Newest, [x])
    -- ->
    --(Agg Oldest, [x])
    -- ->

    _
     -> do  (bs,nms) <- unzip <$> mapM (convertReduce n t) args
            let tys  = fmap (baseType . snd . annotOfExp) args
            let xs   = fmap  CE.XVar           nms
            x' <- convertPrim p (fst $ annotOfExp xx) (baseType ty) (xs `zip` tys)

            nm  <- fresh

            let bs'  = mconcat bs
            let b''  | Pure <- universe ty
                     = pre nm x'
                     | otherwise
                     = post nm x'

            return (bs' <> b'', nm)


 | Nested _ q   <- xx
 = convertQuery n t q
 | Var _ v      <- xx
 = return (mempty, Name v)
 | otherwise
 = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp xx) "convertReduce"

 where
  mkFold ta k z nv
   = do n' <- fresh
        na <- fresh
        let k' = CE.XLam na ta
               $ CE.XLam nv t
               $ k (CE.XVar na) (CE.XVar nv)
        return (red n' $ C.RFold t ta k' z n, n')

  errAggBadArgs
   = lift
   $ Left
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp xx) xx

    
convertExp
        :: Nm   -> T.ValType
        -> Exp (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable)
convertExp nElem t x
 | Var _ (Variable "value") <- x
 = do   n1 <- fresh
        n2 <- fresh
        let fstF    = CE.XLam n1 t
                    $ CE.XLam n2 T.DateTimeT
                    $ CE.XVar n1
        let unpair  = CE.XPrim (C.PrimFold (C.PrimFoldPair t T.DateTimeT) t)
                    CE.@~ fstF
                    CE.@~ CE.XVar nElem
        
        return unpair

 | Just (p, (ann,retty), args) <- takePrimApps x
 = do   args'   <- mapM (convertExp nElem t) args
        let tys  = fmap (baseType . snd . annotOfExp) args
        convertPrim p ann (baseType retty) (args' `zip` tys)

 | Nested _ (Query [] x') <- x
 = convertExp nElem t x'

 | otherwise
 = case x of
    Var (ann,_) n
     -> lift
      $ Left
      $ ConvertErrorExpNoSuchVariable ann n
    Nested (ann,_) q
     -> lift
      $ Left
      $ ConvertErrorExpNestedQueryNotAllowedHere ann q
    App (ann,_) _ _
     -> lift
      $ Left
      $ ConvertErrorExpApplicationOfNonPrimitive ann x
    Prim (ann,retty) p
     -> convertPrim p ann (baseType retty) []


convertArray
        :: a -> Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (CoreBinds Variable, Nm)
convertArray ann _n _t _q
 = lift $ Left $ ConvertErrorTODO ann "convertArray"


convertGroupBy
        :: Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable, C.Exp Variable, C.Exp Variable, T.ValType)
convertGroupBy nElem t q
 = case contexts q of
    []
     | Nested _ qq <- final q
     -> convertGroupBy nElem t qq
     | Just (p, (ann,retty), args) <- takePrimApps $ final q
     -> case p of
         Agg SumA
          | [e] <- args
          -> do let retty' = baseType retty
                e' <- convertExp nElem t e

                n  <- fresh
                let k = CE.XLam n retty'
                      ( CE.XVar n CE.+~ e' )
                let z = CE.constI 0
                x    <- idFun retty'

                return (k, z, x, retty')
          | otherwise
          -> errAggBadArgs

         _
          -> do (ks, zs, xs, ts) <- unzip4 <$> mapM (convertGroupBy nElem t . Query []) args

                (zz, tt) <- pairs zs ts

                let cp ns
                        = convertPrim p ann
                            (baseType retty)
                            ((fmap (uncurry CE.XApp) (xs `zip` ns)) `zip` ts)
                xx       <- unpairs cp ts (baseType retty)

                let applyKs ns = fst <$> pairs (fmap (uncurry CE.XApp) (ks `zip` ns)) ts
                kk       <- unpairs applyKs ts tt

                return (kk, zz, xx, tt)

     | otherwise
      -> errTODO $ annotOfExp $ final q

    (Filter _ e : _)
     -> do  (k,z,x,tt) <- convertGroupBy nElem t q'
            e'         <- convertExp     nElem t e
            prev       <- fresh
            let prev'   = CE.XVar prev
            let k' = CE.XLam prev tt
                   ( CE.XPrim (C.PrimFold C.PrimFoldBool tt)
                     CE.@~ e' CE.@~ (k CE.@~ prev') CE.@~ prev' )
            return (k', z, x, tt)

    (Windowed (ann,_) _ _ : _)
     -> errNotAllowed ann
    (Latest (ann,_) _ : _)
     -> errNotAllowed ann
    (GroupBy (ann,_) _ : _)
     -> errNotAllowed ann
    (Distinct (ann,_) _ : _)
     -> errNotAllowed ann
    (Let (ann,_) _ _ : _)
     -> errNotAllowed ann
    (LetFold (ann,_) _ : _)
     -> errNotAllowed ann


 where
  q' = q { contexts = drop 1 $ contexts q }

  errNotAllowed ann
   = lift $ Left $ ConvertErrorContextNotAllowedInGroupBy ann q
  errTODO ann
   = lift $ Left $ ConvertErrorTODO (fst ann) "convertGroupBy"

  errAggBadArgs
   = lift
   $ Left
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp $ final q) (final q)


  idFun tt = fresh >>= \n -> return (CE.XLam n tt (CE.XVar n))

  pairs (x1:xs) (t1:ts)
   = return
   $ foldl
   (\(xa,ta) (x',t')
    -> ( CE.XPrim
            (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta t')
            CE.@~ xa CE.@~ x'
       , T.PairT ta t'))
   ( x1, t1 )
   ( zip xs ts )

  pairs _ _
   -- TODO: this should be "unit"
   = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp $ final q) "convert GroupBy - constructing pairs for extract"


  unpairs f [tx,ty] ret
   = do nx <- fresh
        ny <- fresh

        nl <- fresh

        f' <- f [CE.XVar nx, CE.XVar ny]

        let xx = CE.XLam nl (T.PairT tx ty)
               ( CE.XPrim (C.PrimFold (C.PrimFoldPair tx ty) ret)
                 CE.@~ (CE.XLam nx tx $ CE.XLam ny ty $ f')
                 CE.@~ CE.XVar nl)
        return xx

  unpairs _ _ _
   = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp $ final q) "convert GroupBy - destructing pairs for extract"

