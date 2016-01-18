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



checkStream
        :: Ord n
        => Env n Type
        -> Stream a n
        -> Either (StreamError a n) (Env n Type)
checkStream se s
 = case s of
    SFold nm t z k
     -> do  tz <- checkX se z
            se'<- insertOrDie StreamErrorNameNotUnique se nm (funOfVal t)
            tk <- checkX se' k
            requireSame (StreamErrorTypeError z)
                        (funOfVal t) tz
            requireSame (StreamErrorTypeError k)
                        (funOfVal t) tk
            return se'

    SFilter x ss
     -> do  tx <- checkX se x
            requireSame (StreamErrorTypeError x)
                        (funOfVal BoolT) tx

            foldM       checkStream se  ss
 where
  checkX env x
   = first StreamErrorExp $ typeExp coreFragmentWorkerFun env x

