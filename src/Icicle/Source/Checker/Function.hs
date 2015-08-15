-- | Typecheck and generalise functions
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Function (
    checkF
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Constraint

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.Either
import qualified        Control.Monad.Trans.State as State

import qualified        Data.Map                as Map
import qualified        Data.Set                as Set

checkF  :: Ord n
        => Map.Map (Name n) (FunctionType n)
        -> Function a n
        -> EitherT (CheckError a n) (Fresh.Fresh n)
                   (Function (Annot a n) n, FunctionType n)

checkF env fun
 = evalGen (checkF' fun) (CheckState env [])

checkF' :: Ord n
        => Function a n
        -> Gen a n (Function (Annot a n) n, FunctionType n)

checkF' fun
 = do mapM_ bindArg $ arguments fun
      (q',_)  <- generateQ    $ body      fun

      ctx <- lift $ lift State.get
      let constrs = ordNub
                  $ fmap snd
                  $ stateConstraints ctx

      args <- mapM lookupArg $ arguments fun

      let argTs = fmap (annResult . fst) args
      let resT  = annResult $ annotOfQuery q'

      let binds = Set.toList
                $ Set.unions
                $ freeT resT : fmap freeT argTs

      let funT  = FunctionType binds constrs argTs resT

      return (Function args q', funT)
 where
  bindArg (_,n)
   = freshType >>= bind n

  freshType
   =    Temporality <$> (TypeVar <$> fresh)
   <*> (Possibility <$> (TypeVar <$> fresh)
   <*>                  (TypeVar <$> fresh))

  lookupArg (a,n)
   = do (_,t,_) <- lookup a n
        return (Annot a t [], n)

