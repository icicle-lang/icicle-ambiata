-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Check (
    checkProgram
  , checkStatement
  , statementContext
  , initialContext
  , Context(..)
  , AccType(..)
  , ProgramError(..)
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

data Context n
 = Context
 { ctxExp :: Env n Type
 , ctxAcc :: Env n AccType }
 deriving (Show, Eq, Ord)

initialContext :: Ord n => Program n p -> Context n
initialContext p
 = Context
 { ctxExp = Map.singleton (binddate p) (FunT [] DateTimeT)
 , ctxAcc = Map.empty }

-- TODO:
--  - check that there is only one fact loop, and it is not inside another loop
--  - unique names: proper unique
checkProgram
        :: Ord n
        => Fragment p
        -> Program n p
        -> Either (ProgramError n p) Type
checkProgram frag p
 = do   let ctx = initialContext p
        ret <- checkStatement frag ctx (statements p)
        case ret of
         Just r    -> return r
         Nothing   -> Left ProgramErrorNoReturn


checkStatement
        :: Ord n
        => Fragment p
        -> Context n
        -> Statement n p
        -> Either (ProgramError n p) (Maybe Type)
checkStatement frag ctx stmt
 = do ctx' <- statementContext frag ctx stmt
      let go = checkStatement frag ctx'
      case stmt of
        If x stmts elses
         -> do t <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x
               requireSame (ProgramErrorWrongType x) t (FunT [] BoolT)
               thenty <- go stmts
               elsety <- go elses

               case thenty == elsety of
                True  -> return thenty
                False -> Left (ProgramErrorConflictingReturnTypes [thenty, elsety])

        Let _ _ stmts
         -> do go stmts

        ForeachInts _ from to stmts
         -> do tf <- mapLeft ProgramErrorExp
                   $ checkExp frag (ctxExp ctx) from
               tt <- mapLeft ProgramErrorExp
                   $ checkExp frag (ctxExp ctx) to

               requireSame (ProgramErrorWrongType from) tf (FunT [] IntT)
               requireSame (ProgramErrorWrongType to)   tt (FunT [] IntT)

               go stmts

        ForeachFacts _ _ _ _ stmts
         -> go stmts


        Block []
         -> return Nothing
        Block [stmts]
         -> go stmts
        Block (s:ss)
         -> go s >> go (Block ss)

        InitAccumulator _ stmts
         -> go stmts

        Read _ _ stmts
         -> go stmts

        Write n x
         -> do t <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x

               a <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx

               case a of
                ATUpdate accTy
                 -> do requireSame (ProgramErrorWrongType x) t (FunT [] accTy)
                       return Nothing
                _
                 -> Left (ProgramErrorWrongAccumulatorType n)

        Push n x
         -> do t <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x

               a <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx

               case a of
                ATPush elemTy
                 -> do requireSame (ProgramErrorWrongType x) t (FunT [] elemTy)
                       return Nothing
                _
                 -> Left (ProgramErrorWrongAccumulatorType n)

        Return x
         -> do t <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x

               return (Just t)

        KeepFactInHistory
         -> do return Nothing

        LoadResumable n
         -> do _ <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx
               return Nothing

        SaveResumable n
         -> do _ <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx
               return Nothing





statementContext
        :: Ord n
        => Fragment p
        -> Context n
        -> Statement n p
        -> Either (ProgramError n p) (Context n)
statementContext frag ctx stmt
 = case stmt of
    If _ _ _
     -> return ctx

    Let n x _
     -> do t <- mapLeft ProgramErrorExp
              $ checkExp frag (ctxExp ctx) x
           return (ctx { ctxExp = Map.insert n t $ ctxExp ctx })

    ForeachInts n _ _ _
     -> return (ctx { ctxExp = Map.insert n (FunT [] IntT) (ctxExp ctx) })

    ForeachFacts n n' ty _ _
     -> return (ctx { ctxExp = Map.insert n (FunT [] ty) $ Map.insert n' (FunT [] DateTimeT) (ctxExp ctx)})

    Block _
     -> return ctx

    InitAccumulator acc _
     -> do (n, at) <- checkAcc acc
           return (ctx { ctxAcc = Map.insert n at $ ctxAcc ctx })

    Read n acc _
     -> do a <- maybeToRight (ProgramErrorNoSuchAccumulator acc)
              $ Map.lookup acc $ ctxAcc ctx

           case a of
            ATUpdate accTy
             -> return (ctx { ctxExp = Map.insert n (FunT [] accTy) $ ctxExp ctx })
            ATPush accTy
             -> return (ctx { ctxExp = Map.insert n (FunT [] (ArrayT accTy)) $ ctxExp ctx })

    Write _ _
     -> return ctx

    Push _ _
     -> return ctx

    Return _
     -> return ctx

    KeepFactInHistory
     -> return ctx

    LoadResumable _
     -> return ctx

    SaveResumable _
     -> return ctx

 where
  checkAcc (Accumulator n at ty x)
   = case at of
      Latest
       -> do    t <- mapLeft ProgramErrorExp
                   $ checkExp frag (ctxExp ctx) x
                requireSame (ProgramErrorWrongType x) t (FunT [] IntT)
                return (n, ATPush ty)
      Mutable
       -> checkUpdate n x ty

  checkUpdate n x ty
   = do t <- mapLeft ProgramErrorExp
           $ checkExp frag (ctxExp ctx) x
        requireSame (ProgramErrorWrongType x) t (FunT [] ty)
        return (n, ATUpdate ty)

