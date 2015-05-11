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
                => n
                -- ^ We introduce scalar bindings for elements of streams.
                -- Because the stream names might conflict with existing scalar names,
                -- we prefix the names of streams with something.
                -- Suggest "element" or something.
                -> n
                -- ^ As above, this is the "accumulator" prefix.
                -> C.Program n
                -> A.Program n Prim
programFromCore elemPrefix accPrefix p
 = A.Program
 { A.precomps   = C.precomps    p
 -- Create accumulators for each reduce
 , A.accums     = fmap accum (C.reduces p)

 -- Nest the streams into a single loop
 , A.loop       = A.FactLoop (C.input p) (Name elemPrefix)
                $ makeStatements elemPrefix accPrefix
                  (C.streams p) (C.reduces p)

 , A.postdate   = C.postdate    p
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
        => n
        -> n
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        -> [Statement n Prim]
makeStatements elemPrefix accPrefix strs reds
 = let sources = filter ((==Nothing) . CS.inputOfStream . snd) strs
   in  fmap (insertStream elemPrefix accPrefix strs reds) sources


-- | Create statements for given stream, its child streams, and its reduces
insertStream
        :: Ord n
        => n
        -> n
        -> [(Name n, CS.Stream n)]
        -> [(Name n, CR.Reduce n)]
        ->  (Name n, CS.Stream n)
        -> Statement n Prim
insertStream elemPrefix accPrefix strs reds (n, strm)
       -- Get the reduces and their updates
 = let reds' = filter ((==n) . CR.inputOfReduce . snd) reds
       upds  = fmap (statementOfReduce elemPrefix accPrefix) reds'

       -- Get all streams that use this directly as input
       strs' = filter ((==Just n) . CS.inputOfStream . snd) strs
       subs  = fmap   (insertStream elemPrefix accPrefix strs reds)     strs'

       -- All statements together
       alls     = upds <> subs
       
       -- Bind something or other
       allLet x = Let (NameMod elemPrefix n) x     alls

   in case strm of
       -- Sources just bind the input and do their children
       CS.Source
        -> allLet $ XVar $ Name elemPrefix

       -- If within i days
       CS.SourceWindowedDays i
        -> IfWindowed i [allLet $ XVar $ Name elemPrefix]

       -- Filters become ifs
       CS.STrans (CS.SFilter _) x inp
        -> If (x `XApp` XVar (NameMod elemPrefix inp)) [allLet $ XVar $ NameMod elemPrefix inp]

       -- Maps apply given function and then do their children
       CS.STrans (CS.SMap _ _) x inp
        -> allLet $ XApp x $ XVar $ NameMod elemPrefix inp


-- | Get update statement for given reduce
statementOfReduce
        :: n
        -> n
        -> (Name n, CR.Reduce n)
        -> Statement n Prim
statementOfReduce elemPrefix accPrefix (n,r)
 = case r of
    -- Apply fold's konstrukt to current accumulator value and input value
    CR.RFold _ ta k _ inp
     -- Darn - arguments wrong way around!
     -- TODO: need to generate fresh name here
     -- or disable uniqueness checking
     -> let n' = NameMod accPrefix n
        in  Update n (XLam n' ta (k `XApp` (XVar n') `XApp` (XVar $ NameMod elemPrefix inp)))
    -- Push most recent inp
    CR.RLatest _ _ inp
     -> Push n (XVar $ NameMod elemPrefix inp)

