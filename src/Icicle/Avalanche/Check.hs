-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Check (
    checkProgram
  ) where

import              Icicle.Avalanche.Program

import              Icicle.Common.Fragment
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              P

import              Data.Either.Combinators
import qualified    Data.Map    as Map

data ProgramError n p
 = ProgramErrorExp (ExpError n p)
 | ProgramErrorWrongType (Exp n p) Type Type
 | ProgramErrorTODO -- TODO
 deriving (Show, Eq, Ord)

data AccType
 = ATUpdate ValType
 | ATPush   ValType
 deriving (Show, Eq, Ord)

typeOfAccType :: AccType -> Type
typeOfAccType (ATUpdate t) = FunT [] t
typeOfAccType (ATPush   t) = FunT [] (ArrayT t)


checkProgram
        :: Ord n
        => Fragment p
        -> Program n p
        -> Either (ProgramError n p) Type
checkProgram frag p
 = do   pres    <- checkExps Map.empty
                 $ precomps p
        
        accs    <- Map.fromList
               <$> (mapM (checkAcc pres)            
                 $ accums p)

        checkLoop frag pres accs $ loop p

        -- Put the accs in the environment
        let env' = Map.union (Map.map typeOfAccType accs) pres
        let env''= case postdate p of
                   Nothing -> env'
                   Just nm -> Map.insert nm (FunT [] DateTimeT) env'

        posts   <- checkExps env''
                 $ postcomps p

        mapLeft ProgramErrorExp
                 $ checkExp frag posts
                 $ returns p
        
 where
  checkExps env xs
   = mapLeft ProgramErrorExp
   $ foldM checkExp1 env xs

  checkExp1 e (n,x)
   = do t <- checkExp frag e x
        return (Map.insert n t e)

  checkAcc env (Accumulator n at)
   = case at of
      Resumable ty x
       -> checkUpdate env n x ty
      Windowed ty x
       -> checkUpdate env n x ty
      Latest ty x
       -> do    t <- mapLeft ProgramErrorExp
                   $ checkExp frag env x
                requireSame (ProgramErrorWrongType x) t (FunT [] IntT)
                return (n, ATPush ty)

  checkUpdate env n x ty
   = do t <- mapLeft ProgramErrorExp
           $ checkExp frag env x
        requireSame (ProgramErrorWrongType x) t (FunT [] ty)
        return (n, ATUpdate ty)

  
checkLoop
        :: Ord n
        => Fragment p
        -> Env  n Type
        -> Env  n AccType
        -> FactLoop n p
        -> Either (ProgramError n p) ()
checkLoop frag env accs (FactLoop inputType inputBind stmts_)
 = go (Map.insert inputBind streamType env) stmts_

 where
  streamType = FunT [] (PairT inputType DateTimeT)

  go e stmts = mapM_ (checkStmt e) stmts

  checkStmt e s
   = case s of
      If x stmts
       -> do t <- mapLeft ProgramErrorExp
                $ checkExp frag e x
             requireSame (ProgramErrorWrongType x) t (FunT [] BoolT)
             go e stmts

      IfWindowed _ stmts
       ->    go e stmts

      Let n x stmts
       -> do t <- mapLeft ProgramErrorExp
                $ checkExp frag e x
             go (Map.insert n t e) stmts
   
      Update n x
       -> do t <- mapLeft ProgramErrorExp
                $ checkExp frag e x

             a <- maybeToRight ProgramErrorTODO
                $ Map.lookup n accs

             case a of
              ATUpdate accTy
               -> requireSame (ProgramErrorWrongType x) t (FunT [FunT [] accTy] accTy)
              _
               -> Left ProgramErrorTODO
   
      Push n x
       -> do t <- mapLeft ProgramErrorExp
                $ checkExp frag e x

             a <- maybeToRight ProgramErrorTODO
                $ Map.lookup n accs

             case a of
              ATPush elemTy
               -> requireSame (ProgramErrorWrongType x) t (FunT [] elemTy)
              _
               -> Left ProgramErrorTODO


