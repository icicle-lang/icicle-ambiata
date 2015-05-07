-- | Condense the streams in a program,
-- so that source with same window, filters with same predicate, etc
-- are all squashed together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Program.Condense (
    condenseProgram
  ) where

import Icicle.Common.Base
import Icicle.Common.Exp
import Icicle.Core.Program.Program
import Icicle.Core.Reduce.Reduce
import Icicle.Core.Stream.Stream

import              P

-- | Condense the stream operations together
condenseProgram :: Ord n => Program n -> Program n
condenseProgram p
 = let (ss,rs) = go [] (streams p) (reduces p)
   in  p { streams = ss
         , reduces = rs }
 where
  go seen [] reds
   = (seen, reds)

  -- Check if stream is like any we've already seen
  go seen ((n,t):ts) reds
   | ((n',_):_) <- filter (streamEquivalent t . snd) seen
   -- If so, find all mentions of n and replace with the one we've already got
   = let (ts',rs') = substStreamName n n' ts reds
     in  go seen ts' rs'

   -- This is a unique stream so tack it on
   | otherwise
   = go (seen <> [(n,t)]) ts reds


-- | Check if two streams are equivalent
streamEquivalent :: Ord n => Stream n -> Stream n -> Bool
streamEquivalent s s'

 | Source                <- s
 , Source                <- s'
 = True

 | SourceWindowedDays n  <- s
 , SourceWindowedDays n' <- s'
 = n == n'

 | STrans st  x  inp     <- s
 , STrans st' x' inp'    <- s'
 =  st  == st'
 && x `alphaEquality` x'
 && inp == inp'

 -- Must be different constructors
 | otherwise
 = False


-- | Rename all uses of stream to another name
substStreamName :: Eq n => Name n -> Name n -> [(Name n, Stream n)] -> [(Name n, Reduce n)] -> ([(Name n, Stream n)], [(Name n, Reduce n)])
substStreamName from to ss rs
 = ( fmap (onSnd subStream) ss
   , fmap (onSnd subReduce) rs )

 where
  onSnd f (a,b) = (a, f b)

  subStream s
   = case s of
      STrans st x inp
       | inp == from
       -> STrans st x to
      _
       -> s

  subReduce r
   = case r of
      RFold t a k z inp
       | inp == from
       -> RFold t a k z to
      RLatest t x inp
       | inp == from
       -> RLatest t x to
      _
       -> r


