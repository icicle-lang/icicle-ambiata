-- | Typechecking an entire core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Check (
      Program (..)
    , checkProgram
    ) where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp             as X
import              Icicle.Core.Stream          as S
import              Icicle.Core.Reduce          as R

import              Icicle.Core.Program.Program as P
import              Icicle.Core.Program.Error

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map


-- | Check the entire program 
checkProgram
        :: Ord n
        => Program n
        -> Either (ProgramError n) Type
checkProgram p
 = do   -- Check precomputations, starting with an empty environment
        pres    <- checkExps ProgramErrorPre Map.empty    (P.precomps     p)

        -- Check stream computations with precomputations in environment
        stms    <- checkStreams (streamEnv pres (input p)) (P.streams      p)

        -- Check reduces against streams and pres
        reds    <- checkReduces stms                      (P.reduces      p)

        -- Check postcomputations with precomputations and reduces
        post    <- checkExps ProgramErrorPost reds        (P.postcomps    p)

        -- Finally, check the return against the postcomputation environment
        mapLeft ProgramErrorReturn $ checkExp post        (P.returns      p)


-- | Check all expression bindings, collecting up environment as we go
checkExps
        :: Ord n
        => (ExpError n -> ProgramError n)
        -> Env n Type
        -> [(Name n, Exp n)]
        -> Either (ProgramError n) (Env n Type)

checkExps _ env []
 = return env

checkExps err env ((n,x):bs)
 = do   t    <- mapLeft err
              $ checkExp env x
        env' <- insertOrDie ProgramErrorNameNotUnique env n t
        checkExps err env' bs


-- | Check stream bindings, collecting up environment
checkStreams
        :: Ord n
        => StreamEnv n
        -> [(Name n, Stream n)]
        -> Either (ProgramError n) (StreamEnv n)
checkStreams env []
 = return env

checkStreams env ((n,s):bs)
 = do   t   <- mapLeft ProgramErrorStream
             $ checkStream env s
        se' <- insertOrDie ProgramErrorNameNotUnique (S.streams env) n t
        checkStreams (env { S.streams = se' }) bs


-- | Check reduce bindings.
-- Here, we must be careful: the result of reductions is *not* available in later reductions
-- (as this would require multiple iterations).
-- So, collect all reductions and only add them at the end.
-- We also throw away the stream environment, as it will not be used again.
checkReduces
        :: Ord n
        => StreamEnv n
        -> [(Name n, Reduce n)]
        -> Either (ProgramError n) (Env n Type)

checkReduces env []
 = return (scalars env)

checkReduces env ((n,r):bs)
 = do   t   <- mapLeft ProgramErrorReduce
             $ checkReduce env r

        -- Now check the rest with the original environment
        env'  <- checkReduces env bs
        -- then insert into the environment
        env'' <- insertOrDie ProgramErrorNameNotUnique env' n (funOfVal t)

        -- And we're done
        return env''

