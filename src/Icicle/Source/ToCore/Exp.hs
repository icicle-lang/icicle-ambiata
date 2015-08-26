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
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Type          as T
import                  Icicle.Common.Fresh

import                  P

import                  Control.Monad.Trans.Class

import                  Data.List (zip)

import qualified        Data.Map as Map



-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: Ord n
        => Exp (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExp x
 = case x of
    Var ann n
     -> do  fs <- convertFeatures
            case Map.lookup n fs of
             Just (_, x')
              -> (x' . CE.XVar ()) <$> convertInputName
             -- Variable must be bound as a precomputation
             Nothing
              -> CE.XVar () <$> convertFreshenLookup (annAnnot ann) n


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
     -> do  pats'  <- mapM (\(p,alt) -> (,) p <$> convertExp alt) pats
            scrut' <- convertExp scrut
            scrutT <- convertValType (annAnnot ann) $ annResult $ annotOfExp scrut
            resT   <- convertValType (annAnnot ann) $ annResult ann
            convertCase x scrut' pats' scrutT resT



convertExpQ
        :: Ord n
        => Query (Annot a n) n
        -> ConvertM a n (C.Exp () n)
convertExpQ q
 = case contexts q of
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



convertCase
        :: Ord n
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
                     CE.@~ (CE.xLam n ta som) CE.@~ non
                     CE.@~ scrut)

         T.BoolT
          | Just ([],tru) <- Map.lookup ConTrue     m
          , Just ([],fal) <- Map.lookup ConFalse    m
          -> return ((CE.xPrim $ C.PrimFold C.PrimFoldBool resT)
                     CE.@~ tru CE.@~ fal
                     CE.@~ scrut)

         T.PairT ta tb
          | Just ([na,nb],tup) <- Map.lookup ConTuple   m
          , xfst <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst ta tb) CE.@~ CE.xVar sn
          , xsnd <- CE.xPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd ta tb) CE.@~ CE.xVar sn
          -> return ( CE.XLet () sn scrut
                    $ CE.XLet () na xfst
                    $ CE.XLet () nb xsnd
                    $ tup)

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
   = convertFreshenAdd n
  mkVars (PatCon _ _)
   = convertError $ ConvertErrorBadCaseNestedConstructors (annAnnot $ annotOfExp x) x

