{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Check (
      checkReduce
    ) where

import              Icicle.Core.Type
import              Icicle.Core.Exp

import              Icicle.Core.Reduce.Reduce
import              Icicle.Core.Reduce.Error
import              Icicle.Core.Stream.Check

import              P

import              Data.Either.Combinators


checkReduce
        :: Ord n
        => StreamEnv n
        -> Reduce n
        -> Either (ReduceError n) ValType
checkReduce se r
 = case r of
    RFold k z n
     -> do  inp <- lookupOrDie ReduceErrorNoSuchStream (streams se) n
            kty <- checkX k
            zty <- checkX z
            case zty of
             FunT [] acc
              -> do  expect k kty (FunT [funOfVal acc, inp] acc)
                     return acc

             _
              -> Left (ReduceErrorTypeError z Nothing zty)

    RLatest num n
     -> do  inp <- lookupOrDie ReduceErrorNoSuchStream (streams se) n
            numty <- checkX num
            expect num numty (funOfVal IntT)
            case inp of
             FunT [] i
              -> return i
             _
              -> Left (ReduceErrorStreamIsFunType n)
            
 where
  checkX
   = mapLeft ReduceErrorExp . checkExp (pres se)

  expect xx actual expected
   | actual == expected
   = return ()
   | otherwise
   = Left (ReduceErrorTypeError xx (Just expected) actual)

