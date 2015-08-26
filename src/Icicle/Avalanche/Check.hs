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

data ProgramError a n p
 = ProgramErrorExp  (ExpError a n p)
 | ProgramErrorWrongType (Exp a n p) Type Type
 | ProgramErrorNoSuchAccumulator    (Name n)
 | ProgramErrorWrongAccumulatorType (Name n)
 | ProgramErrorMultipleFactLops
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

initialContext :: Ord n => Program a n p -> Context n
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
        -> Program a n p
        -> Either (ProgramError a n p) ()
checkProgram frag p
 = do   let ctx = initialContext p
        checkStatement frag ctx (statements p)


checkStatement
        :: Ord n
        => Fragment p
        -> Context n
        -> Statement a n p
        -> Either (ProgramError a n p) ()
checkStatement frag ctx stmt
 = do ctx' <- statementContext frag ctx stmt
      let go = checkStatement frag ctx'
      case stmt of
        If x stmts elses
         -> do t <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x
               requireSame (ProgramErrorWrongType x) t (FunT [] BoolT)
               go stmts
               go elses

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


        Block stmts
         -> mapM_ go stmts

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
                       return ()
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
                       return ()
                _
                 -> Left (ProgramErrorWrongAccumulatorType n)

        Output _ x
         -> do _ <- mapLeft ProgramErrorExp
                  $ checkExp frag (ctxExp ctx) x
               return ()

        KeepFactInHistory
         -> do return ()

        LoadResumable n
         -> do _ <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx
               return ()

        SaveResumable n
         -> do _ <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                  $ Map.lookup n $ ctxAcc ctx
               return ()





statementContext
        :: Ord n
        => Fragment p
        -> Context n
        -> Statement a n p
        -> Either (ProgramError a n p) (Context n)
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

    Output _ _
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

