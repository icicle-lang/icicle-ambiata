-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.FromCore (
    programFromCore
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp

import              Icicle.Core.Exp.Prim
import              Icicle.Avalanche.Program    as A
import qualified    Icicle.Core.Program.Program as C
import qualified    Icicle.Core.Reduce          as CR
import qualified    Icicle.Core.Stream          as CS

import              P

-- | Convert an entire program to Avalanche
programFromCore :: Ord n
                => (Name n -> Name n)
                -- ^ We introduce scalar bindings for elements of streams.
                -- Because the stream names might conflict with existing scalar names,
                -- we prefix the names of streams with something
                -> C.Program n
                -> A.Program n Prim
programFromCore elemName p
 = A.Program
 { A.precomps   = C.precomps    p
 -- Create accumulators for each reduce
 , A.accums     = fmap accum (C.reduces p)

 -- Nest the streams into a single loop
 , A.loop       = A.Loop (C.input p)
                $ makeStatements elemName
                  (C.streams p) (C.reduces p)

 , A.postcomps  = C.postcomps   p
 , A.returns    = C.returns     p
 }
 where
  -- Create a latest accumulator
  accum (n, CR.RLatest ty x _)
   = A.Accumulator n (A.Latest ty x)
  
  -- Fold accumulator
  accum (n, CR.RFold _ ty _ x inp)
   -- If it's windowed, create windowed accumulator
   | CS.isStreamWindowed (C.streams p) inp
   = A.Accumulator n (A.Windowed ty x)
   -- Not windowed, so resumable fold
   | otherwise
   = A.Accumulator n (A.Resumable ty x)


-- | Starting from an empty list of statements,
-- repeatedly insert each stream into the statements wherever it fits
makeStatements
        :: Ord n
        => (Name n -> Name n)
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        -> [Statement n Prim]
makeStatements elemName strs reds
 = let sources = filter ((==Nothing) . CS.inputOfStream . snd) strs
   in  concatMap (insertStream elemName strs reds) sources


-- | Create statements for given stream, its child streams, and its reduces
insertStream
        :: Ord n
        => (Name n -> Name n)
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        ->  (Name n, CS.Stream n)
        -> [Statement n Prim]
insertStream elemName strs reds (n, strm)
       -- Get the reduces and their updates
 = let reds' = filter ((==n) . CR.inputOfReduce . snd) reds
       upds  = fmap (statementOfReduce elemName) reds'

       -- Get all streams that use this directly as input
       strs' = filter ((==Just n) . CS.inputOfStream . snd) strs
       subs  = concatMap (insertStream elemName strs reds) strs'

       -- All statements together
       alls     = upds <> subs
       
       -- The sources need a name to refer to the input by
       allSrc   = UseSource (elemName n) : alls
       -- Bind something or other
       allLet x = Let (elemName n) x : alls

   in case strm of
       -- Sources just bind the input and do their children
       CS.Source
        -> allSrc

       -- If within i days
       CS.SourceWindowedDays i
        -> [IfWindowed i allSrc]

       -- Filters become ifs
       CS.STrans (CS.SFilter _) x inp
        -> [If (x `XApp` XVar (elemName inp)) $ allLet $ XVar $ elemName inp]

       -- Maps apply given function and then do their children
       CS.STrans (CS.SMap _ _) x inp
        -> allLet $ XApp x $ XVar $ elemName inp


-- | Get update statement for given reduce
statementOfReduce
        :: (Name n -> Name n)
        -> (Name n, CR.Reduce n)
        -> Statement n Prim
statementOfReduce elemName (n,r)
 = case r of
    -- Apply fold's konstrukt to current accumulator value and input value
    CR.RFold _ _ k _ inp
     -> Update n (k `XApp` (XVar n) `XApp` (XVar $ elemName inp))
    -- Push most recent inp
    CR.RLatest _ _ inp
     -> Push n (XVar $ elemName inp)

