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
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base


import                  P

import                  Control.Monad.Trans.Class
import                  Data.List (zip, unzip)


type Nm = Name Variable

convertQueryTop
        :: QueryTop (a,UniverseType) Variable
        -> ConvertM a Variable (C.Program Variable)
convertQueryTop qt
 = do   inp <- fresh
        -- TODO: look this up in context
        let inpTy = T.IntT

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
            nmap    <- fresh
            nval    <- fresh
     
            (f,z)   <- convertGroupBy ann nval nt q'
            e'      <- convertExp         nval nt e

            let mapt = T.MapT t1 t2

            let insertOrUpdate
                  = CE.XLam nmap mapt
                  $ CE.XLam nval nt
                  ( CE.XPrim (C.PrimMap $ C.PrimMapInsertOrUpdate t1 t2)
                    CE.@~ (f CE.@~ CE.XVar nval)
                    CE.@~ (f CE.@~ CE.XVar nval CE.@~ z)
                    CE.@~  e'
                    CE.@~ CE.XVar nmap )

            let r = red n' 
                  $ C.RFold nt mapt insertOrUpdate 
                  ( CE.emptyMap t1 t2)
                    n

            return (r, n')

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
 = case (p, args) of
    (Agg Count, [])
     -> mkFold T.IntT (\na _nv -> na CE.+~ CE.constI 1) (CE.constI 0)
    (Agg SumA, [])
     -> mkFold T.IntT (\na nv -> na CE.+~ nv) (CE.constI 0)


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


 | Nested _ q <- xx
 = convertQuery n t q
 | otherwise
 = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp xx) "convertReduce"

 where
  mkFold ta k z
   = do n' <- fresh
        na <- fresh
        nv <- fresh
        let k' = k (CE.XVar na) (CE.XVar nv)
        return (red n' $ C.RFold t ta k' z n, n')

    
convertExp
        :: Nm   -> T.ValType
        -> Exp (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable)
convertExp nElem t x
 | Var _ (Variable "value") <- x
 = return (CE.XVar nElem)

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
        :: a -> Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable, C.Exp Variable)
convertGroupBy ann _nElem _t _q
 = lift $ Left $ ConvertErrorTODO ann "convertGroupBy"


