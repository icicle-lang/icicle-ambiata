-- | Basic things for basic people
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PatternGuards              #-}
module Icicle.Source.Checker.Base (
    CheckEnv (..)
  , Invariants (..)
  , emptyCheckEnv
  , emptyInvariants

  , CheckOptions (..)
  , optionBigData
  , optionSmallData
  , defaultCheckOptions

  , GenEnv, GenConstraintSet
  , Gen(..)
  , Query'C
  , Exp'C
  , evalGen

  , require
  , discharge
  , fresh
  , introForalls
  , lookup
  , bindT
  , withBind
  , removeElementBinds

  , substE
  , substTQ
  , substTX
  , substAnnot

  ) where

import           Icicle.Source.Checker.Error

import           Icicle.Source.Query
import           Icicle.Source.Type

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh         as Fresh

import           P

import           Control.Monad.Trans.Class

import           Data.Functor.Identity
import qualified Data.Map                    as Map
import           Data.Hashable (Hashable)

import           X.Control.Monad.Trans.Either


-- | Type checking environment.
-- Keep track of all the things we need to know
data CheckEnv a n
 = CheckEnv {
 -- | Mapping from variable names to whole types
   checkEnvironment :: Map.Map (Name n) (FunctionType n)
 -- | Function bodies
 , checkBodies      :: Map.Map (Name n) (Function a n)
 , checkInvariants  :: Invariants
 }

-- | Typechecking invariants that aren't checked by the type system.
--   i.e. unimplemented things.
--
data Invariants
 = Invariants {
 -- | Unimplemented in Core: windows inside groups/windows/group-folds/latests
   allowWindows    :: Bool
 -- | Unimplemented in Core: group-folds inside groups/windows/group-folds/latests
 , allowGroupFolds :: Bool
 -- | Unimplemented in Core: latest inside latest
 , allowLatest     :: Bool
 }

-- | Initial environment at top-level, not inside a group, and allowing contexts
emptyCheckEnv :: CheckEnv a n
emptyCheckEnv
 = CheckEnv Map.empty Map.empty emptyInvariants

emptyInvariants :: Invariants
emptyInvariants = Invariants True True True

--------------------------------------------------------------------------------


data CheckOptions
 = CheckOptions
 { checkOptionRequireResumable :: Bool
 , checkOptionNowPure          :: Bool
 } deriving (Show, Eq, Ord)

optionBigData :: CheckOptions
optionBigData = CheckOptions True True

optionSmallData :: CheckOptions
optionSmallData = CheckOptions False True

defaultCheckOptions :: CheckOptions
defaultCheckOptions = optionSmallData

--------------------------------------------------------------------------------


type GenEnv n             = Map.Map (Name n) (FunctionType n)
type GenConstraintSet a n = [(a, Constraint n)]

newtype Gen a n t
 = Gen { constraintGen :: EitherT (CheckError a n) (Fresh.Fresh n) t }
 deriving (Functor, Applicative, Monad)

evalGen
    :: Gen a n t
    -> EitherT (CheckError a n) (Fresh.Fresh n) t
evalGen f
 = EitherT
 $ Fresh.FreshT
 $ \ns -> Identity
 $ flip Fresh.runFresh ns
 $ runEitherT
 $ constraintGen f

type Query'C a n = Query (Annot a n) n
type Exp'C   a n = Exp   (Annot a n) n


-- | Add a constraint to the context.
require :: a -> Constraint n -> GenConstraintSet a n
require a c = [(a,c)]

-- | Discharge the constraints in some context after applying some type substitutions.
--
discharge
  :: (Hashable n, Eq n)
  => (q -> a)
  -> (SubstT n -> q -> q)
  -> (q, SubstT n, GenConstraintSet a n)
  -> Gen a n (q, SubstT n, GenConstraintSet a n)
discharge annotOf sub (q, s, conset)
 = do let cs = nubConstraints $ fmap (\(a,c) -> (a, substC s c)) conset
      case dischargeCS cs of
       Left errs
        -> Gen . hoistEither
         $ errorNoSuggestions (ErrorConstraintsNotSatisfied (annotOf q) errs)
       Right (s', cs')
        -> do let s'' = compose s s'
              --let e' = fmap (substFT s'') env
              return (sub s'' q, s'', cs')

fresh :: Hashable n => Gen a n (Name n)
fresh
 = Gen . lift $ Fresh.fresh

-- | Freshen function type by applying introduction rules to foralls.
--
introForalls
  :: (Hashable n, Eq n)
  => a
  -> FunctionType n
  -> Gen a n (FunctionType n, [Type n], Type n, GenConstraintSet a n)
introForalls ann f
 = do freshen <- Map.fromList <$> mapM mkSubst (functionForalls f)

      let cons = concat
               $ fmap (require ann . substC freshen)
               $ functionConstraints f

      let sub   = substT freshen
      return ( f
             , fmap sub $ functionArguments f
             ,      sub $ functionReturn    f
             , cons )
 where
  mkSubst n
   = ((,) n . TypeVar) <$> fresh

-- | Look up a name in the context. Return the original type, along with the argument
--   types and return type where forall-quantified variables have been freshen'd.
--
lookup
  :: (Hashable n, Eq n)
  => a
  -> Name n
  -> GenEnv n
  -> Gen a n (FunctionType n, [Type n], Type n, GenConstraintSet a n)
lookup ann n env
 = case Map.lookup n env of
     Just t
      -> introForalls ann t
     Nothing
      -> Gen . hoistEither
       $ errorSuggestions (ErrorNoSuchVariable ann n)
                           [AvailableBindings n $ Map.toList env]

-- | Bind a new to a type in the given context.
bindT :: (Hashable n, Eq n) => Name n -> Type n -> GenEnv n -> GenEnv n
bindT n t
 = Map.insert n (function0 t)

-- | Temporarily add the binding to a context, then do something.
withBind
  :: (Hashable n, Eq n)
  => Name n
  -> Type n
  -> GenEnv n
  -> (GenEnv n -> Gen a n r)
  -> Gen a n r
withBind n t old gen
 = gen (bindT n t old)

removeElementBinds :: (Hashable n, Eq n) => GenEnv n -> GenEnv n
removeElementBinds env
 = let elts  = Map.keys $ Map.filter isElementTemporality env
   in  foldr Map.delete env elts
 where
  isElementTemporality ft
   = getTemporalityOrPure (functionReturn ft) == TemporalityElement


substE :: (Hashable n, Eq n) => SubstT n -> GenEnv n -> GenEnv n
substE s
 = fmap (substFT s)

substTQ :: (Hashable n, Eq n) => SubstT n -> Query'C a n -> Query'C a n
substTQ s
 = reannotQ (substAnnot s)

substTX :: (Hashable n, Eq n) => SubstT n -> Exp'C a n -> Exp'C a n
substTX s
 = reannotX (substAnnot s)

substAnnot :: (Hashable n, Eq n) => SubstT n -> Annot a n -> Annot a n
substAnnot s ann
 = ann
 { annResult = substT s $ annResult ann
 , annConstraints = nubConstraints $ fmap (\(a,c) -> (a, substC s c)) $ annConstraints ann
 }
