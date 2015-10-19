{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Check (
      checkReduce
    ) where

import              Icicle.Common.Type
import              Icicle.Core.Exp

import              Icicle.Core.Reduce.Reduce
import              Icicle.Core.Reduce.Error
import              Icicle.Core.Stream.Check

import              P

import              Data.Either.Combinators


checkReduce
        :: Ord n
        => StreamEnv n
        -> Reduce a n
        -> Either (ReduceError a n) ValType
checkReduce se r
 = case r of
    RFold t a k z n
     -> do  inp <- lookupOrDie ReduceErrorNoSuchStream (streams se) n
            kty <- checkX k
            zty <- checkX z

            requireSame (ReduceErrorTypeError k)
                        (funOfVal t)                     (funOfVal inp)
            requireSame (ReduceErrorTypeError k)
                        (FunT [funOfVal a, funOfVal t] a) kty
            requireSame (ReduceErrorTypeError z)
                        (funOfVal a)                      zty

            return a

 where
  checkX
   = mapLeft ReduceErrorExp . typeExp coreFragmentWorkerFun (scalars se)

