-- | Convert Element Expressions to Core
--
-- Worker functions, eg the "value * 2" in "sum (value * 2)".
-- Aggregates are dealt with elsewhere.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Exp (
    convertExp
  , convertExpQ
  , convertCase
  , convertCaseFreshenPat

  , isAnnotPossibly
  , unwrapSum
  , rewrapSum
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Type          as T
import                  Icicle.Common.Base
import                  Icicle.Common.Fresh

import                  P

import                  Control.Monad.Trans.Class

import                  Data.List (zip)
import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)



-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: (Hashable n, Eq n)
        => Exp (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExp x
 = case x of
    Var ann n
     -> do  bound <- convertFreshenLookupMaybe n
            fs <- featureContextVariables <$> convertFeatures
            case bound of
             Just fv
              -> return $ CE.XVar () fv
             _ 
              | Just fv <- Map.lookup n fs
              -> (featureVariableExp fv . CE.XVar ()) <$> convertInputName
              | otherwise
              -> convertError $ ConvertErrorExpNoSuchVariable (annAnnot ann) n


    Nested _ q
     -> convertExpQ q


    App ann _ _
     -- Primitive application: convert arguments, then convert primitive
     | Just (p, Annot { annResult = resT }, args) <- takePrimApps x
     -> do  args'   <- mapM convertExp args
            let tys  = fmap (annResult . annotOfExp) args
            convertPrim p (annAnnot ann) resT (args' `zip` tys)

     | otherwise
     -> convertError
      $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot ann) x


    Prim ann p
     -> convertPrim p (annAnnot ann) (annResult ann) []

    -- Only deal with flattened, single layer cases.
    -- We need a pass beforehand to simplify them.
    Case ann scrut pats
     -> do  scrut' <- convertExp scrut
            pats'  <- mapM goPat pats
            scrutT <- convertValType (annAnnot ann) $ annResult $ annotOfExp scrut
            resT   <- convertValType (annAnnot ann) $ annResult ann
            convertCase x scrut' pats' scrutT resT

 where
  goPat (p,alt)
   = convertContext
   $ do p'   <- convertCaseFreshenPat p
        alt' <- convertExp alt
        return (p', alt')


convertExpQ
        :: (Hashable n, Eq n)
        => Query (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExpQ q
 -- Remove any new bindings from context afterwards
 = convertContext
 $ case contexts q of
    []
     -> convertExp $ final q
    (Let _ b d:cs)
     -> do  d' <- convertExp d
            -- NB: because it's non-recursive let, the freshen must be done after the definition
            b' <- convertFreshenAdd b
            x' <- convertExpQ $ Query cs $ final q
            return $ CE.XLet () b' d' x'
    _
     -> convertError
      $ ConvertErrorExpNestedQueryNotAllowedHere (annAnnot $ annotOfQuery q) q


-- | Enfreshinate the variables in a case pattern and add them to the convert environment.
--
convertCaseFreshenPat :: (Hashable n, Eq n) => Pattern n -> ConvertM a n (Pattern n)
convertCaseFreshenPat p
 = case p of
    PatCon c ps
      -> PatCon c <$> mapM convertCaseFreshenPat ps
    PatDefault
      -> return PatDefault
    PatVariable n
      -> PatVariable <$> convertFreshenAdd n


convertCase
        :: Hashable n
        => Exp (Annot a n) n
        -> C.Exp () n
        -> [(Pattern n, C.Exp () n)]
        -> T.ValType
        -> T.ValType
        -> ConvertM a n (C.Exp () n)
convertCase x scrut pats scrutT resT
 = do   sn <- lift fresh
        m <- convertConstructorMap
        case scrutT of
         T.OptionT ta
          | Just ([n],som) <- Map.lookup ConSome    m
          , Just ([],non)  <- Map.lookup ConNone    m

          -> return ((CE.xPrim $ C.PrimFold (C.PrimFoldOption ta) resT)
                     CE.@~ (CE.xLam n ta som) CE.@~ (CE.xLam sn T.UnitT non)
                     CE.@~ scrut)

         T.BoolT
          | Just ([],tru) <- Map.lookup ConTrue     m
          , Just ([],fal) <- Map.lookup ConFalse    m
          -> return ((CE.xPrim $ C.PrimFold C.PrimFoldBool resT)
                     CE.@~ CE.xLam sn T.UnitT tru
                     CE.@~ CE.xLam sn T.UnitT fal
                     CE.@~ scrut)

         T.PairT ta tb
          | Just ([na,nb],tup) <- Map.lookup ConTuple   m
          , xfst <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst ta tb) CE.@~ CE.xVar sn
          , xsnd <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd ta tb) CE.@~ CE.xVar sn
          -> return ( CE.XLet () sn scrut
                    $ CE.XLet () na xfst
                    $ CE.XLet () nb xsnd
                    $ tup)

         T.SumT ta tb
          | Just ([nl],xl)  <- Map.lookup ConLeft    m
          , Just ([nr],xr)  <- Map.lookup ConRight   m

          -> return ((CE.xPrim $ C.PrimFold (C.PrimFoldSum ta tb) resT)
                     CE.@~ (CE.xLam nl ta xl)
                     CE.@~ (CE.xLam nr tb xr)
                     CE.@~ scrut)

         _
          -> convertError $ ConvertErrorBadCaseNoDefault (annAnnot $ annotOfExp x) x
 where
  convertConstructorMap
   = Map.fromList <$> mapM mkPatMap pats

  mkPatMap (PatCon c ps, alt)
   = do ps'  <- mapM mkVars ps
        return (c, (ps', alt))
  mkPatMap _
   = convertError $ ConvertErrorBadCaseNoDefault (annAnnot $ annotOfExp x) x

  mkVars PatDefault
   = lift fresh
  mkVars (PatVariable n)
   = return n
  mkVars (PatCon _ _)
   = convertError $ ConvertErrorBadCaseNestedConstructors (annAnnot $ annotOfExp x) x


isAnnotPossibly :: Annot a n -> Bool
isAnnotPossibly ann
 = case getPossibilityOrDefinitely (annResult ann) of
    PossibilityPossibly -> True
    _                   -> False


unwrapSum
    :: Bool
    -> T.ValType
    -> Name n
    -> C.Exp () n
    -> Name n
    -> T.ValType
    -> C.Exp () n
    -> C.Exp () n
unwrapSum isPossibly rett nErr x nk t bodyx
 | T.SumT T.ErrorT ty <- t
 , T.SumT T.ErrorT ret' <- rett
 -- We can only do this for (Sum Error)s introduced by Reify:
 -- not ones that the programmer explicitly wrote
 , isPossibly
 = CE.makeApps () (CE.xPrim $ C.PrimFold (C.PrimFoldSum T.ErrorT ty) rett)
 [ CE.xLam nErr T.ErrorT ( CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstLeft T.ErrorT ret')
                         [ CE.xVar nErr ])
 , CE.xLam nk ty bodyx
 , x ]
 | otherwise
 = CE.xLet nk x bodyx


rewrapSum
    :: Bool
    -> T.ValType
    -> C.Exp () n
    -> C.Exp () n
rewrapSum isPossibly rett bodyx
 | T.SumT T.ErrorT ret' <- rett
 , isPossibly
 = CE.makeApps () (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstRight T.ErrorT ret')
 [ bodyx ]
 | otherwise
 = bodyx
