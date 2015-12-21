-- | Condense the streams in a program,
-- so that source with same window, filters with same predicate, etc
-- are all squashed together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wwarn #-}
module Icicle.Core.Program.Condense (
    condenseProgram
  , condenseStreams
  ) where

import Icicle.Common.Base
import Icicle.Common.Exp
import Icicle.Core.Program.Program
import Icicle.Core.Stream.Stream

import              P

-- | Condense streams then reductions
condenseProgram :: Ord n => a -> Program a n -> Program a n
condenseProgram _ = condenseStreams

-- | Condense the stream operations together
-- XXX TODO fix this after Core rejig
condenseStreams :: Ord n => Program a n -> Program a n
condenseStreams p
 = p
{-
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
-}


{-
-- | Check if two streams are equivalent
streamEquivalent :: Ord n => Stream a n -> Stream a n -> Bool
streamEquivalent s s'

 | Source                <- s
 , Source                <- s'
 = True

 | SWindow t  newer  older  inp  <- s
 , SWindow t' newer' older' inp' <- s'
 =  t   == t'
 && newer == newer'
 && older == older'
 && inp == inp'

 | STrans st  x  inp     <- s
 , STrans st' x' inp'    <- s'
 =  st  == st'
 && x `alphaEquality` x'
 && inp == inp'

 -- Must be different constructors
 | otherwise
 = False



-- | Rename all uses of stream to another name
substStreamName :: Eq n
                => Name n
                -> Name n
                -> [(Name n, Stream a n)]
                -> [(Name n, Reduce a n)]
                -> ([(Name n, Stream a n)], [(Name n, Reduce a n)])
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
      SWindow t x mx inp
       | inp == from
       -> SWindow t x mx to
      _
       -> s

  subReduce r
   = case r of
      RFold t a k z inp
       | inp == from
       -> RFold t a k z to
      _
       -> r

-}

