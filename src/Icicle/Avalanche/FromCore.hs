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

import              Icicle.Core.Exp.Prim

import              Icicle.Avalanche.Statement.Statement as A
import              Icicle.Avalanche.Program    as A
import qualified    Icicle.Core.Program.Program as C
import qualified    Icicle.Core.Program.Check   as C
import qualified    Icicle.Core.Stream          as CS

import              P
import              Data.String
import qualified    Data.Map as Map

import              Data.Functor.Identity


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

namerText :: IsString a => (a -> n) -> Namer n
namerText f
 = Namer (NameMod (f (fromString "elem")))
         (NameMod (f (fromString "acc")))


-- | Convert an entire program to Avalanche
programFromCore :: Ord n
                => Namer n
                -> C.Program () n
                -> A.Program () n Prim
programFromCore namer p
 = A.Program
 { A.input
    = C.inputType p
 , A.bindtime
    = C.snaptimeName p
 , A.statements
    = lets (C.precomps p)
    -- $ accums (filter (readFromHistory.snd) $ C.reduces p)
    -- ( factLoopHistory    <>
    ( createAccums
    ( readaccum (C.inputName p, inputType') (initAccums (C.streams p)) <>
      mconcat (fmap loadResumables accNames) <>
      factLoopNew                            <>
      mconcat (fmap saveResumables accNames) <>
      readaccums
    ( lets (C.postcomps p) outputs) ))
 }
 where
  -- TODO: currently ignoring resumables and KeepFactInHistory
  -- resumables = filter (not.readFromHistory.snd) $ C.reduces p

  lets stmts inner
   = foldr (\(n,x) a -> Let n x a) inner stmts

  -- Fold accumulator
  initAccum (CS.SFold n ty z _) inner
   = let write | z /= X.XValue () ty (defaultOfType ty)
               = Write (namerAccPrefix namer n) z
               | otherwise
               = mempty

         sub   = runIdentity $ X.transformX return (return . X.renameExp ren) inner
         ren n'= if n == n' then namerElemPrefix namer n else n'

     in write <> Read (namerElemPrefix namer n) (namerAccPrefix namer n) ty sub
  initAccum (CS.SFilter _ ss) inner
   = foldr initAccum inner ss
  initAccums ss
   = foldr initAccum mempty ss

  createAccums inner
   = foldr createAccumMkInit inner accNames
  createAccumMkInit (n,ty)
   = InitAccumulator $ A.Accumulator (namerAccPrefix namer n) ty
   $ X.XValue () ty (defaultOfType ty)

  -- Nest the streams into a single loop
  factLoopNew
   = factLoop FactLoopNew

  factLoop loopType
   = ForeachFacts [(C.inputName p, inputType')] inputType' loopType
   $ Block factStmts

  inputType' = PairT (C.inputType p) TimeT

  (accNames, factStmts)
   = makeStatements p namer (C.streams p)

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

  readaccums inner
   = foldr readaccum inner accNames


-- | Starting from an empty list of statements,
-- repeatedly insert each stream into the statements wherever it fits
makeStatements
        :: Ord n
        => C.Program () n
        -> Namer n
        -> [CS.Stream () n]
        -> ([(Name n, ValType)], [Statement () n Prim])
makeStatements p namer streams
 = let (nms,stms) = go [] streams
   in  ((C.inputName p, PairT (C.inputType p) TimeT) : nms, Write (namerAccPrefix namer $ C.inputName p) (X.XVar () $ C.inputName p) : stms)
 where
  go nms []
   = (nms, [])
  go nms (s:strs)
   = let (nms', s') = makeStatement nms s
         (nms'',ss') = go nms' strs
     in  (nms'', s' : ss')

  makeStatement nms strm
   = case strm of
      CS.SFold n t _ k
       -> let nms' = (n,t) : nms
          in  (nms', mkReads nms' (Write (namerAccPrefix namer n) k))
      CS.SFilter x ss
       -> let (nms', ss') = go nms ss
          in  (nms', mkReads nms $ If x (Block ss') mempty)

  mkReads [] inner
   = inner
  mkReads ((n,t):ns) inner
   = mkReads ns (Read n (namerAccPrefix namer n) t inner)

