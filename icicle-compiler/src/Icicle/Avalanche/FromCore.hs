-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.FromCore (
    programFromCore
  , Namer(..)
  , namerText
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp as X
import              Icicle.Common.Fresh

import              Icicle.Core.Exp.Prim

import              Icicle.Avalanche.Statement.Statement as A
import              Icicle.Avalanche.Program    as A
import qualified    Icicle.Core.Program.Program as C
import qualified    Icicle.Core.Program.Check   as C
import qualified    Icicle.Core.Stream          as CS

import              P
import              Data.String
import qualified    Data.Map as Map
import qualified    Data.Set as Set

import              Data.Hashable (Hashable)


data Namer n
 = Namer
 { namerElemPrefix :: Name n -> Name n
 -- ^ We introduce scalar bindings for elements of streams.
 -- Because the stream names might conflict with existing scalar names,
 -- we prefix the names of streams with something.
 -- Suggest "element" or something.
 , namerAccPrefix  :: Name n -> Name n
 -- ^ As above, this is the "accumulator" prefix.
 }

namerText :: (IsString a, Hashable n) => (a -> n) -> Namer n
namerText f
 = Namer (modName (f (fromString "elem")))
         (modName (f (fromString "acc")))


-- | Convert an entire program to Avalanche
programFromCore :: (Hashable n, Eq n)
                => Namer n
                -> C.Program () n
                -> Fresh n (A.Program () n Prim)
programFromCore namer p
 = do   (accNames, factStmts) <- makeStatements p namer (C.streams p)

        let inputType'  = PairT (C.inputType p) TimeT
        let factBinds   = FactBinds (C.factTimeName p) (C.factIdName p) [(C.factValName p, inputType')]
        let factLoopNew = ForeachFacts factBinds inputType' FactLoopNew
                        $ Block factStmts
        let inner       = mconcat (fmap loadResumables accNames) <>
                          factLoopNew                            <>
                          mconcat (fmap saveResumables accNames) <>
                          readaccums accNames (lets (C.postcomps p) outputs)
                -- accums (filter (readFromHistory.snd) $ C.reduces p)
                -- ( factLoopHistory    <>

        accs <- createAccums Map.empty (C.streams p) inner
        let stmts       = lets (C.precomps p) accs

        return (A.Program
                { A.input       = C.inputType p
                , A.bindtime    = C.snaptimeName p
                , A.statements  = stmts })

 where
  -- TODO: currently ignoring resumables and KeepFactInHistory
  -- resumables = filter (not.readFromHistory.snd) $ C.reduces p

  lets stmts inner
   = foldr (\(n,x) a -> Let n x a) inner stmts

  createAccums _ [] inner
   = return inner
  createAccums subst (CS.SFold n ty z _ : ss) inner
   = do  z' <- X.subst () subst z
         let subst' = Map.insert n z' subst
         rest <- createAccums subst' ss inner
         return $ InitAccumulator ( A.Accumulator (namerAccPrefix namer n) ty z' ) rest
  createAccums subst (CS.SFilter _ ss : ss') inner
   = createAccums subst (ss <> ss') inner


  outputExps
   = Map.fromList (C.returns p)

  outputTypes
   = Map.fromList
   $ mapMaybe (\(n,t) -> (,) <$> pure n <*> fromFunT t)
   $ either (const []) id (C.checkProgram p)

  fromFunT (FunT [] t) = Just t
  fromFunT _           = Nothing

  outputs
   = Block
   $ Map.elems
   $ Map.intersectionWithKey (\n x t -> A.Output n t [(x, t)]) outputExps outputTypes

  loadResumables (n, ty)
   = LoadResumable (namerAccPrefix namer n) ty

  saveResumables (n, ty)
   = SaveResumable (namerAccPrefix namer n) ty

  readaccum (n, ty)
   = Read n (namerAccPrefix namer n) ty

  readaccums accNames inner
   = foldr readaccum inner accNames


-- | Starting from an empty list of statements,
-- repeatedly insert each stream into the statements wherever it fits
makeStatements
        :: (Hashable n, Eq n)
        => C.Program () n
        -> Namer n
        -> [CS.Stream () n]
        -> Fresh n ([(Name n, ValType)], [Statement () n Prim])
makeStatements _p namer streams
 = go [] streams
 where
  go nms []
   = return (nms, [])
  go nms (s:strs)
   = do (nms', s') <- makeStatement nms s
        (nms'',ss') <- go nms' strs
        return (nms'', s' : ss')

  makeStatement nms strm
   = case strm of
      CS.SFold n t _ k
       -> do let nms' = (n,t) : nms
             s <- mkReads nms' (Write (namerAccPrefix namer n)) k
             return (nms', s)
      CS.SFilter x ss
       -> do (nms', ss') <- go nms ss
             -- Substatements are not bound in predicate, so use nms instead of nms'
             s <- mkReads nms (\x' -> If x' (Block ss') mempty) x
             return  (nms', s)

  mkReads nms inner k
   = mkReads' nms inner k Map.empty (X.freevars k)

  mkReads' [] inner k subs _
   = do k' <- X.subst () subs k
        return $ inner $ k'

  mkReads' ((n,t):ns) inner k subs fvs
   | Set.member n fvs
   = do n' <- freshPrefixBase $ nameBase n
        let subs' = Map.insert n (X.XVar () n') subs
        let acc = namerAccPrefix namer n
        mkReads' ns (\x -> Read n' acc t (inner x)) k subs' fvs
   | otherwise
   = mkReads' ns inner k subs fvs

