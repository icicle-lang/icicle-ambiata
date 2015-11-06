-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Check (
    checkProgram
  , ProgramError(..)
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Program

import              Icicle.Common.Annot
import              Icicle.Common.Base
import              Icicle.Common.Fragment
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              P

import              Data.Either.Combinators
import qualified    Data.List   as List
import qualified    Data.Map    as Map

data ProgramError a n p
 = ProgramErrorExp  (ExpError a n p)
 | ProgramErrorWrongType (Exp a n p) Type Type
 | ProgramErrorNoSuchAccumulator    (Name n)
 | ProgramErrorWrongAccumulatorType (Name n)
 | ProgramErrorWrongValType         (Name n) ValType ValType
 | ProgramErrorMultipleFactLops
 deriving (Show, Eq, Ord)

data Context n
 = Context
 { ctxExp :: Env n Type
 , ctxAcc :: Env n ValType }
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
        -> Either (ProgramError a n p) (Program (Annot a) n p)
checkProgram frag p
 = do   let ctx = initialContext p
        ss <- checkStatement frag ctx (statements p)
        return p { statements = ss }


checkStatement
        :: Ord n
        => Fragment p
        -> Context n
        -> Statement a n p
        -> Either (ProgramError a n p) (Statement (Annot a) n p)
checkStatement frag ctx stmt
 = do ctx' <- statementContext frag ctx stmt
      let go = checkStatement frag ctx'
      case stmt of
        If x stmts elses
         -> do x' <- mapLeft ProgramErrorExp
                   $ checkExp frag (ctxExp ctx) x

               let t = annType (annotOfExp x')

               requireSame (ProgramErrorWrongType x) t (FunT [] BoolT)

               If x' <$> go stmts
                     <*> go elses

        Let n x stmts
         -> do x' <- mapLeft ProgramErrorExp
                   $ checkExp frag (ctxExp ctx) x
               Let n x' <$> go stmts

        ForeachInts n from to stmts
         -> do from' <- mapLeft ProgramErrorExp
                      $ checkExp frag (ctxExp ctx) from
               to'   <- mapLeft ProgramErrorExp
                      $ checkExp frag (ctxExp ctx) to

               let tf = annType (annotOfExp from')
                   tt = annType (annotOfExp to')

               requireSame (ProgramErrorWrongType from) tf (FunT [] IntT)
               requireSame (ProgramErrorWrongType to)   tt (FunT [] IntT)

               ForeachInts n from' to' <$> go stmts

        ForeachFacts ns vt lo stmts
         -> ForeachFacts ns vt lo <$> go stmts


        Block stmts
         -> Block <$> mapM go stmts

        InitAccumulator acc stmts
         -> do acc'   <- checkAccumulator frag ctx acc
               stmts' <- go stmts
               return (InitAccumulator acc' stmts')

        Read n acc vt stmts
         -> Read n acc vt <$> go stmts

        Write n x
         -> do x'   <- mapLeft ProgramErrorExp
                     $ checkExp frag (ctxExp ctx) x

               vt   <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                     $ Map.lookup n $ ctxAcc ctx

               let t = annType (annotOfExp x')

               requireSame (ProgramErrorWrongType x) t (FunT [] vt)
               return (Write n x')

        Output n t xts
         -> do let xs = fmap fst xts
                   ts = fmap snd xts

               xs' <- mapLeft ProgramErrorExp
                    $ traverse (checkExp frag (ctxExp ctx)) xs

               let xts' = List.zip xs' ts

               return (Output n t xts')

        KeepFactInHistory
         -> return KeepFactInHistory

        LoadResumable n t
         -> do t' <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                   $ Map.lookup n $ ctxAcc ctx

               requireSame (ProgramErrorWrongValType n) t t'
               return (LoadResumable n t)

        SaveResumable n t
         -> do t' <- maybeToRight (ProgramErrorNoSuchAccumulator n)
                   $ Map.lookup n $ ctxAcc ctx

               requireSame (ProgramErrorWrongValType n) t t'
               return (SaveResumable n t)


checkAccumulator
        :: Ord n
        => Fragment p
        -> Context n
        -> Accumulator a n p
        -> Either (ProgramError a n p) (Accumulator (Annot a) n p)
checkAccumulator frag ctx (Accumulator n ty x)
 = do x' <- mapLeft ProgramErrorExp
          $ checkExp frag (ctxExp ctx) x

      let t = annType (annotOfExp x')

      requireSame (ProgramErrorWrongType x) t (FunT [] ty)
      return (Accumulator n ty x')


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
              $ typeExp frag (ctxExp ctx) x
           return (ctx { ctxExp = Map.insert n t $ ctxExp ctx })

    ForeachInts n _ _ _
     -> return (ctx { ctxExp = Map.insert n (FunT [] IntT) (ctxExp ctx) })

    ForeachFacts ns _ _ _
     -> return (ctx { ctxExp = foldr (\(n, ty) m -> Map.insert n (FunT [] ty) m) (ctxExp ctx) ns })

    Block _
     -> return ctx

    InitAccumulator acc _
     -> do (n, avt) <- checkAcc acc
           return (ctx { ctxAcc = Map.insert n avt $ ctxAcc ctx })

    Read n _ vt _
     -> return (ctx { ctxExp = Map.insert n (FunT [] vt) $ ctxExp ctx })

    Write _ _
     -> return ctx

    Output _ _ _
     -> return ctx

    KeepFactInHistory
     -> return ctx

    LoadResumable _ _
     -> return ctx

    SaveResumable _ _
     -> return ctx

 where
  checkAcc acc@(Accumulator n ty _)
   = do _ <- checkAccumulator frag ctx acc
        return (n, ty)


