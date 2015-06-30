-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Check (
    checkProgram
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Program

import              Icicle.Common.Base
import              Icicle.Common.Fragment
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              P

import              Data.Either.Combinators
import qualified    Data.Map    as Map

data ProgramError n p
 = ProgramErrorExp (ExpError n p)
 | ProgramErrorWrongType (Exp n p) Type Type
 | ProgramErrorNoSuchAccumulator (Name n)
 | ProgramErrorWrongAccumulatorType (Name n)
 | ProgramErrorMultipleFactLops
 | ProgramErrorNoReturn
 | ProgramErrorConflictingReturnTypes [Maybe Type]
 deriving (Show, Eq, Ord)

data AccType
 = ATUpdate ValType
 | ATPush   ValType
 deriving (Show, Eq, Ord)


-- TODO:
--  - check that there is only one fact loop, and it is not inside another loop
--  - unique names: proper unique
checkProgram
        :: Ord n
        => Fragment p
        -> Program n p
        -> Either (ProgramError n p) Type
checkProgram frag p
 = do   let xh = Map.singleton (binddate p) (FunT [] DateTimeT)
        let ah = Map.empty
        ret <- checkStatement frag xh ah (statements p)
        case ret of
         Just r    -> return r
         Nothing   -> Left ProgramErrorNoReturn


checkStatement
        :: Ord n
        => Fragment p
        -> Env  n Type
        -> Env  n AccType
        -> Statement n p
        -> Either (ProgramError n p) (Maybe Type)
checkStatement frag xh ah stmt
 = case stmt of
    If x stmts elses
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag xh x
           requireSame (ProgramErrorWrongType x) t (FunT [] BoolT)
           thenty <- go xh stmts
           elsety <- go xh elses
           
           case thenty == elsety of
            True  -> return thenty
            False -> Left (ProgramErrorConflictingReturnTypes [thenty, elsety])

    Let n x stmts
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag xh x
           go (Map.insert n t xh) stmts

    ForeachInts n from to stmts 
     -> do tf <- mapLeft ProgramErrorExp
               $ checkExp frag xh from
           tt <- mapLeft ProgramErrorExp
               $ checkExp frag xh to

           requireSame (ProgramErrorWrongType from) tf (FunT [] IntT)
           requireSame (ProgramErrorWrongType to)   tt (FunT [] IntT)
           
           go (Map.insert n (FunT [] IntT) xh) stmts

    ForeachFacts n ty stmts 
     -> go (Map.insert n (FunT [] (PairT ty DateTimeT)) xh) stmts


    Block []
     -> return Nothing
    Block [stmts]
     -> go xh stmts
    Block (s:ss)
     -> go xh s >> go xh (Block ss)

    InitAccumulator acc stmts
     -> do (n, at) <- checkAcc acc
           checkStatement frag xh (Map.insert n at ah) stmts

    Read n acc stmts
     -> do a <- maybeToRight (ProgramErrorNoSuchAccumulator acc)
              $ Map.lookup acc ah

           case a of
            ATUpdate accTy
             -> go (Map.insert n (FunT [] accTy) xh) stmts
            ATPush accTy
             -> go (Map.insert n (FunT [] (ArrayT accTy)) xh) stmts

    Write n x
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag xh x

           a <- maybeToRight (ProgramErrorNoSuchAccumulator n)
              $ Map.lookup n ah

           case a of
            ATUpdate accTy
             -> do requireSame (ProgramErrorWrongType x) t (FunT [] accTy)
                   return Nothing
            _
             -> Left (ProgramErrorWrongAccumulatorType n)
 
    Push n x
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag xh x

           a <- maybeToRight (ProgramErrorNoSuchAccumulator n)
              $ Map.lookup n ah

           case a of
            ATPush elemTy
             -> do requireSame (ProgramErrorWrongType x) t (FunT [] elemTy)
                   return Nothing
            _
             -> Left (ProgramErrorWrongAccumulatorType n)

    Return x
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag xh x
        
           return (Just t)

    KeepFactInHistory
     -> do return Nothing





 where
  go xh' = checkStatement frag xh' ah

  checkAcc (Accumulator n at ty x)
   = case at of
      Resumable
       -> checkUpdate n x ty
      Windowed
       -> checkUpdate n x ty
      Latest
       -> do    t <- mapLeft ProgramErrorExp
                   $ checkExp frag xh x
                requireSame (ProgramErrorWrongType x) t (FunT [] IntT)
                return (n, ATPush ty)
      Mutable
       -> checkUpdate n x ty

  checkUpdate n x ty
   = do t <- mapLeft ProgramErrorExp
           $ checkExp frag xh x
        requireSame (ProgramErrorWrongType x) t (FunT [] ty)
        return (n, ATUpdate ty)

  
