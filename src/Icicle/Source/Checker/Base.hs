-- | Basic things for basic people
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Base (
    CheckState (..)
  , Annot(..)
  , annotDiscardConstraints
  , Gen
  , Query'C
  , Exp'C
  , evalGen

  , require
  , discharge
  , fresh
  , freshenFunction
  , lookup
  , bind

  , substTQ
  , substTX
  , substAnnot

  ) where

import                  Icicle.Source.Checker.Error

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.Either
import qualified        Control.Monad.Trans.State as State
import                  Data.Functor.Identity

import qualified        Data.Map                as Map



data CheckState a n
 = CheckState
 { environment :: Map.Map n (FunctionType n)
 , constraints :: [(a, Constraint n)]
 }

type Gen a n t
 = EitherT
    (CheckError a n)
    (Fresh.FreshT n
        (State.State (CheckState a n)))
    t

evalGen
    :: Gen a n t
    -> CheckState a n
    -> EitherT (CheckError a n) (Fresh.Fresh n) t
evalGen gen env
 = EitherT
 $ Fresh.FreshT
 $ \ns -> Identity
 $ flip State.evalState env
 $ flip Fresh.runFreshT ns
 $ runEitherT
 $ gen


data Annot a n
 = Annot
 { annAnnot         :: a
 , annResult        :: Type n
 , annConstraints   :: [(a, Constraint n)]
 }


annotDiscardConstraints :: Annot a n -> (a, Type n)
annotDiscardConstraints ann
 = (annAnnot ann, annResult ann)

type Query'C a n = Query (Annot a n) n
type Exp'C   a n = Exp   (Annot a n) n


require :: a -> Constraint n -> Gen a n ()
require a c
 = lift
 $ lift
 $ do   e <- State.get
        State.put (e { constraints = (a,c) : constraints e })

discharge :: Ord n => (q -> a) -> (SubstT n -> q -> q) -> Gen a n q -> Gen a n q
discharge ann sub g
 = do q <- g
      e <- lift $ lift State.get
      case dischargeCS (constraints e) of
       Left errs
        -> hoistEither $ errorNoSuggestions (ErrorConstraintsNotSatisfied (ann q) errs)
       Right (s, cs')
        -> do let e' = CheckState (fmap (substFT s) $ environment e) cs'
              lift $ lift $ State.put e'
              return $ sub s q

fresh :: Gen a n (Name n)
fresh
 = lift $ Fresh.fresh



freshenFunction :: Ord n => a -> FunctionType n -> Gen a n ([Type n], Type n, FunctionType n)
freshenFunction ann f
 = do   freshen <- Map.fromList <$> mapM mkSubst (functionForalls f)

        mapM_ (require ann . substC freshen) (functionConstraints f)

        let sub = substT freshen
        return ( fmap sub $ functionArguments f
               ,      sub $ functionReturn    f
               ,                              f)

 where
  mkSubst n
   = ((,) n . TypeVar) <$> fresh


lookup :: Ord n => a -> n -> Gen a n ([Type n], Type n, FunctionType n)
lookup ann n
 = do   e <- lift $ lift $ State.get
        let env = environment e
        case Map.lookup n env of
         Just t
          -> freshenFunction ann t
         Nothing
          -> hoistEither
           $ errorSuggestions (ErrorNoSuchVariable ann n)
                              [AvailableBindings $ Map.toList env]

bind :: Ord n => n -> Type n -> Gen a n ()
bind n t
 = lift
 $ lift
 $ do   e <- State.get
        State.put (e { environment = Map.insert n (function0 t) (environment e) })


substTQ :: Ord n => SubstT n -> Query'C a n -> Query'C a n
substTQ s
 = reannotQ (substAnnot s)


substTX :: Ord n => SubstT n -> Exp'C a n -> Exp'C a n
substTX s
 = reannotX (substAnnot s)


substAnnot :: Ord n => SubstT n -> Annot a n -> Annot a n
substAnnot s ann
 = ann
 { annResult = substT s $ annResult ann
 , annConstraints = fmap (\(a,c) -> (a, substC s c)) $ annConstraints ann
 }


