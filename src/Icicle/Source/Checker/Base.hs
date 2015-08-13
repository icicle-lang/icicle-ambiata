-- | Basic things for basic people
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Base (
    CheckEnv (..)
  , Invariants (..)
  , emptyCheckEnv
  , emptyInvariants

  , CheckState (..)
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
  , withBind

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


-- | Type checking environment.
-- Keep track of all the things we need to know
data CheckEnv a n
 = CheckEnv
 -- | Mapping from variable names to whole types
 { checkEnvironment :: Map.Map n (FunctionType n)
 -- | Function bodies
 , checkBodies      :: Map.Map n (Function a n)
 , checkInvariants  :: Invariants
 }

-- | Typechecking invariants that aren't checked by the type system.
data Invariants
 = Invariants
 -- | We can't have windows or other group-like things inside groups.
 -- This is actually treating all of latests, distincts and groups as "group-like"
 -- because they all require compilation to a single fold.
 { allowWindowsOrGroups :: Bool
 }

-- | Initial environment at top-level, not inside a group, and allowing contexts
emptyCheckEnv :: CheckEnv a n
emptyCheckEnv
 = CheckEnv Map.empty Map.empty emptyInvariants

emptyInvariants :: Invariants
emptyInvariants = Invariants True

-- | State needed when doing constraint generation checking
data CheckState a n
 = CheckState
 { stateEnvironment :: Map.Map n (FunctionType n)
 , stateConstraints :: [(a, Constraint n)]
 }
 deriving (Eq, Ord, Show)

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


type Query'C a n = Query (Annot a n) n
type Exp'C   a n = Exp   (Annot a n) n


require :: a -> Constraint n -> Gen a n ()
require a c
 = lift
 $ lift
 $ do   e <- State.get
        State.put (e { stateConstraints = (a,c) : stateConstraints e })

discharge :: Ord n => (q -> a) -> (SubstT n -> q -> q) -> Gen a n q -> Gen a n q
discharge ann sub g
 = do q <- g
      e <- lift $ lift State.get
      case dischargeCS (stateConstraints e) of
       Left errs
        -> hoistEither $ errorNoSuggestions (ErrorConstraintsNotSatisfied (ann q) errs)
       Right (s, cs')
        -> do let e' = CheckState (fmap (substFT s) $ stateEnvironment e) cs'
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
        let env = stateEnvironment e
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
        State.put (e { stateEnvironment = Map.insert n (function0 t) (stateEnvironment e) })

withBind :: Ord n => n -> Type n -> Gen a n r -> Gen a n r
withBind n t gen
 = do   old <- lift $ lift $ State.get
        bind n t
        r   <- gen

        new <- lift $ lift $ State.get
        let env'
             | Just o' <- Map.lookup n (stateEnvironment old)
             = Map.insert n o' $ stateEnvironment new
             | otherwise
             = Map.delete n    $ stateEnvironment new
        lift $ lift $ State.put $ new { stateEnvironment = env' }

        return r


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
 , annConstraints = nubConstraints $ fmap (\(a,c) -> (a, substC s c)) $ annConstraints ann
 }


