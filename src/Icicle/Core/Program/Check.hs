-- | Typechecking an entire core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Check (
      checkProgram
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp             as X
import              Icicle.Core.Stream          as S

import              Icicle.Core.Program.Program as P
import              Icicle.Core.Program.Error

import              P

import qualified    Data.Map as Map
import              Data.Hashable (Hashable)


-- | Check the entire program
checkProgram
        :: (Hashable n, Eq n)
        => Program a n
        -> Either (ProgramError a n) [(OutputName, Type)]
checkProgram p
 = do   -- Check precomputations, starting with an empty environment
        let env0 = Map.singleton (snaptimeName p) (funOfVal $ TimeT)
        pres    <- checkExps ProgramErrorPre env0 (P.precomps     p)
        pres'   <- insertOrDie ProgramErrorNameNotUnique pres (inputName    p) (funOfVal $ PairT (inputType p) TimeT)
               >>= insertOrDie ProgramErrorNameNotUnique pres (factIdName   p) (funOfVal $ FactIdentifierT)

        -- Check stream computations with precomputations in environment
        stms    <- checkStreams pres' (P.streams      p)

        -- Check postcomputations with precomputations and reduces
        post    <- checkExps ProgramErrorPost stms       (P.postcomps    p)

        -- Finally, check the returns against the postcomputation environment
        let checkRet (n,x)
                = do t' <- first ProgramErrorReturn
                         $ typeExp coreFragment post x
                     case t' of
                      FunT [] _
                       -> return (n, t')
                      FunT (_:_) _
                       -> Left (ProgramErrorReturnNotValueType t')

        mapM checkRet (P.returns p)



-- | Check all expression bindings, collecting up environment as we go
checkExps
        :: (Hashable n, Eq n)
        => (ExpError a n Prim -> ProgramError a n)
        -> Env n Type
        -> [(Name n, Exp a n)]
        -> Either (ProgramError a n) (Env n Type)

checkExps _ env []
 = return env

checkExps err env ((n,x):bs)
 = do   t    <- first err
              $ typeExp coreFragment env x
        env' <- insertOrDie ProgramErrorNameNotUnique env n t
        checkExps err env' bs


-- | Check stream bindings, collecting up environment
checkStreams
        :: (Hashable n, Eq n)
        => Env n Type
        -> [Stream a n]
        -> Either (ProgramError a n) (Env n Type)
checkStreams env []
 = return env

checkStreams env (b:bs)
 = do   e'  <- first ProgramErrorStream
             $ checkStream env b
        checkStreams e' bs

