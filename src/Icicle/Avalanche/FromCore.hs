-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.FromCore (
    programFromCore
  , Namer(..)
  , namerText
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Type

import              Icicle.Core.Exp.Prim
import              Icicle.Core.Exp.Combinators

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
    $ accums
    ( factLoop <>
      readaccums
    ( lets (makepostdate <> C.postcomps p) returnStmt) )
 }
 where
  lets stmts inner
   = foldr (\(n,x) a -> Let n x a) inner stmts

  accums inner
   = foldr (\ac s -> InitAccumulator (accum ac) s)
            inner
           (C.reduces p)

  -- Nest the streams into a single loop
  factLoop
   = ForeachFacts (namerFact namer) (C.input p)
   $ Block
   $ makeStatements namer (C.input p)
                                       (C.streams p) (C.reduces p)

  returnStmt
   = A.Return (C.returns p)

  -- Create a latest accumulator
  accum (n, CR.RLatest ty x _)
   = A.Accumulator (namerAccPrefix namer n) A.Latest ty x
  
  -- Fold accumulator
  accum (n, CR.RFold _ ty _ x inp)
   -- If it's windowed, create windowed accumulator
   | CS.isStreamWindowed (C.streams p) inp
   = A.Accumulator (namerAccPrefix namer n) A.Windowed ty x
   -- Not windowed, so resumable fold
   | otherwise
   = A.Accumulator (namerAccPrefix namer n) A.Resumable ty x

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
       upds  = fmap (statementOfReduce namer) reds'

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
       CS.SourceWindowedDays i
        -> let unpair    = XPrim (PrimFold (PrimFoldPair inputType DateTimeT) BoolT)
               factValue = namerElemPrefix namer (namerFact namer)
               factDate  = namerElemPrefix namer (namerDate namer)
               nowDate   = namerDate namer
               diff      = XPrim (PrimDateTime PrimDateTimeDaysDifference)

               check  = XLam factValue  inputType
                      $ XLam factDate   DateTimeT
                      $ (diff @~ XVar factDate @~ XVar nowDate) <=~ constI i

               window = unpair @~ check @~ XVar (namerFact namer)
               
           in If window (allLet $ XVar $ namerFact namer)
                         mempty

       -- Filters become ifs
       CS.STrans (CS.SFilter _) x inp
        -> If (x `XApp` XVar (namerElemPrefix namer inp))
              (allLet $ XVar $ namerElemPrefix namer inp)
               mempty

       -- Maps apply given function and then do their children
       CS.STrans (CS.SMap _ _) x inp
        -> allLet $ XApp x $ XVar $ namerElemPrefix namer inp


-- | Get update statement for given reduce
statementOfReduce
        :: Namer n
        -> (Name n, CR.Reduce n)
        -> Statement n Prim
statementOfReduce namer (n,r)
 = case r of
    -- Apply fold's konstrukt to current accumulator value and input value
    CR.RFold _ _  k _ inp
     -- Darn - arguments wrong way around!
     -> let n' = namerAccPrefix namer n
        in  Read n' n'
          $ Write n' (k `XApp` (XVar n') `XApp` (XVar $ namerElemPrefix namer inp))
    -- Push most recent inp
    CR.RLatest _ _ inp
     -> Push (namerAccPrefix namer n) (XVar $ namerElemPrefix namer inp)

