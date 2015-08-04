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
import              Icicle.Common.Exp
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Simp.Beta as Beta

import              Icicle.Core.Exp.Prim
import              Icicle.Core.Exp.Combinators

import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Avalanche.Statement.Statement as A
import              Icicle.Avalanche.Program    as A
import qualified    Icicle.Core.Program.Program as C
import qualified    Icicle.Core.Reduce          as CR
import qualified    Icicle.Core.Stream          as CS

import              P
import              Data.Text (Text)


data Namer n
 = Namer
 { namerElemPrefix :: Name n -> Name n
 -- ^ We introduce scalar bindings for elements of streams.
 -- Because the stream names might conflict with existing scalar names,
 -- we prefix the names of streams with something.
 -- Suggest "element" or something.
 , namerAccPrefix  :: Name n -> Name n
 -- ^ As above, this is the "accumulator" prefix.
 , namerDate       :: Name n
 , namerFact       :: Name n
 }

namerText :: (Text -> n) -> Namer n
namerText f
 = Namer (NameMod (f "elem"))
         (NameMod (f "acc"))
         (NameMod (f "gen") $ Name (f "date"))
         (NameMod (f "gen") $ Name (f "fact"))


-- | Convert an entire program to Avalanche
programFromCore :: Ord n
                => Namer n
                -> C.Program n
                -> A.Program n Prim
programFromCore namer p
 = A.Program
 { A.binddate
    = namerDate namer
 , A.statements
    = lets (C.precomps p)
    $ accums (filter (readFromHistory.snd) $ C.reduces p)
    ( factLoopHistory    <>
    ( accums resumables
    ( mconcat (fmap loadResumables resumables) <>
      factLoopNew               <>
      mconcat (fmap saveResumables resumables) <>
      readaccums
    ( lets (makepostdate <> C.postcomps p) returnStmt) )))
 }
 where
  resumables = filter (not.readFromHistory.snd) $ C.reduces p

  lets stmts inner
   = foldr (\(n,x) a -> Let n x a) inner stmts

  accums reds inner
   = foldr (\ac s -> InitAccumulator (accum ac) s)
            inner
           reds

  factLoopHistory
   = factLoop FactLoopHistory (filter (readFromHistory.snd) $ C.reduces p)

  readFromHistory r
   = case r of
      CR.RLatest{} -> True
      CR.RFold _ _ _ _ inp -> CS.isStreamWindowed (C.streams p) inp

  -- Nest the streams into a single loop
  factLoopNew
   = factLoop FactLoopNew (C.reduces p)

  factLoop loopType reduces
   = ForeachFacts (namerElemPrefix namer $ namerFact namer) (namerElemPrefix namer $ namerDate namer) (C.input p) loopType
   $ Let (namerFact namer)
        (XPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair (C.input p) $ ValType DateTimeT)
        `XApp` (XVar $ namerElemPrefix namer $ namerFact namer)
        `XApp` (XVar $ namerElemPrefix namer $ namerDate namer))
   $ Block
   $ makeStatements namer (C.input p) (C.streams p) reduces

  returnStmt
   = A.Return (C.returns p)

  -- Create a latest accumulator
  accum (n, CR.RLatest ty x _)
   = A.Accumulator (namerAccPrefix namer n) A.Latest ty x

  -- Fold accumulator
  accum (n, CR.RFold _ ty _ x _)
   = A.Accumulator (namerAccPrefix namer n) A.Mutable ty x

  loadResumables (n, CR.RFold{})
   = LoadResumable $ namerAccPrefix namer n
  loadResumables _
   = mempty

  saveResumables (n, CR.RFold{})
   = SaveResumable $ namerAccPrefix namer n
  saveResumables _
   = mempty

  readaccums inner 
   = foldr (\ac s -> Read (fst ac) (namerAccPrefix namer $ fst ac) s)
            inner
           (C.reduces p)


  makepostdate
   = case C.postdate p of
      Nothing -> []
      Just nm -> [(nm, XVar $ namerDate namer)]


-- | Starting from an empty list of statements,
-- repeatedly insert each stream into the statements wherever it fits
makeStatements
        :: Ord n
        => Namer n
        -> ValType
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        -> [Statement n Prim]
makeStatements namer inputType strs reds
 = let sources = filter ((==Nothing) . CS.inputOfStream . snd) strs
   in  fmap (insertStream namer inputType strs reds) sources


-- | Create statements for given stream, its child streams, and its reduces
insertStream
        :: Ord n
        => Namer n
        -> ValType
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        ->  (Name n, CS.Stream n)
        -> Statement n Prim
insertStream namer inputType strs reds (n, strm)
       -- Get the reduces and their updates
 = let reds' = filter ((==n) . CR.inputOfReduce . snd) reds
       upds  = fmap (statementOfReduce namer strs) reds'

       -- Get all streams that use this directly as input
       strs' = filter ((==Just n) . CS.inputOfStream . snd) strs
       subs  = fmap   (insertStream namer inputType strs reds)     strs'

       -- All statements together
       alls     = Block (upds <> subs)

       -- Bind some element
       allLet x = Let (namerElemPrefix namer n) x     alls

   in case strm of
       -- Sources just bind the input and do their children
       CS.Source
        -> allLet $ XVar $ namerFact namer

       -- If within i days
       CS.SWindow _ newerThan olderThan inp
        -> let factDate  = namerElemPrefix namer (namerDate namer)
               nowDate   = namerDate namer
               diff      = XPrim (PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeDaysDifference)

               check  | Just o' <- olderThan
                      = XPrim (PrimMinimal $ Min.PrimLogical Min.PrimLogicalAnd)
                        @~ (diff @~ XVar factDate @~ XVar nowDate) <=~ newerThan
                        @~ (diff @~ XVar factDate @~ XVar nowDate) >=~ o'

                      | otherwise
                      = (diff @~ XVar factDate @~ XVar nowDate) <=~ newerThan

               else_  | Just o' <- olderThan
                      = If ((diff @~ XVar factDate @~ XVar nowDate) <~ o' )
                           KeepFactInHistory
                           mempty

                      | otherwise
                      = mempty

           in If check (allLet $ XVar $ namerElemPrefix namer inp)
                         else_

       -- Filters become ifs
       CS.STrans (CS.SFilter _) x inp
        -> If (Beta.betaToLets (x `XApp` XVar (namerElemPrefix namer inp)))
              (allLet $ XVar $ namerElemPrefix namer inp)
               mempty

       -- Maps apply given function and then do their children
       CS.STrans (CS.SMap _ _) x inp
        -> allLet $ Beta.betaToLets $ XApp x $ XVar $ namerElemPrefix namer inp


-- | Get update statement for given reduce
statementOfReduce
        :: Ord n
        => Namer n
        -> [(Name n, CS.Stream n)]
        -> (Name n, CR.Reduce n)
        -> Statement n Prim
statementOfReduce namer strs (n,r)
 = case r of
    -- Apply fold's konstrukt to current accumulator value and input value
    CR.RFold _ _  k _ inp
     -> let n' = namerAccPrefix namer n

            -- If it's windowed, note that we will need this fact in the next snapshot
            k' | CS.isStreamWindowed strs inp
               = KeepFactInHistory
               | otherwise
               = mempty

        in  Read n' n'
          ( Write n' (Beta.betaToLets (k `XApp` (XVar n') `XApp` (XVar $ namerElemPrefix namer inp)))
          <> k' )
    -- Push most recent inp
    CR.RLatest _ _ inp
     -> Push (namerAccPrefix namer n) (XVar $ namerElemPrefix namer inp)

