{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Stream.Check (
      checkStream
    ) where

import              Icicle.Common.Type
import              Icicle.Core.Exp

import              Icicle.Core.Stream.Stream
import              Icicle.Core.Stream.Error

import              P

import              Data.Hashable (Hashable)
import qualified    Data.Map as Map



checkStream
        :: (Hashable n, Eq n)
        => Env n Type
        -> Env n Type
        -> Stream a n
        -> Either (StreamError a n) (Env n Type)
checkStream zenv kenv s
 = case s of
    SFold nm t z k
     -> do  tz <- checkX zenv z
            zenv' <- insertOrDie StreamErrorNameNotUnique zenv nm (funOfVal t)
            tk <- checkX (Map.union zenv' kenv)  k
            requireSame (StreamErrorTypeError z)
                        (funOfVal t) tz
            requireSame (StreamErrorTypeError k)
                        (funOfVal t) tk
            return zenv'

    SFilter x ss
     -> do  tx <- checkX (Map.union zenv kenv) x
            requireSame (StreamErrorTypeError x)
                        (funOfVal BoolT) tx

            foldM       (flip checkStream kenv) zenv ss
 where
  checkX env x
   = first StreamErrorExp $ typeExp coreFragmentWorkerFun env x

