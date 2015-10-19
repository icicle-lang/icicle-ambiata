-- | Condense the streams in a program,
-- so that source with same window, filters with same predicate, etc
-- are all squashed together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Program.Condense (
    condenseProgram
  , condenseStreams
  , condenseReduces
  ) where

import Icicle.Common.Base
import Icicle.Common.Exp
import Icicle.Core.Program.Program
import Icicle.Core.Reduce.Reduce
import Icicle.Core.Stream.Stream

import              P

-- | Condense streams then reductions
condenseProgram :: Ord n => a -> Program a n -> Program a n
condenseProgram a_fresh = condenseReduces a_fresh . condenseStreams

-- | Condense the stream operations together
condenseStreams :: Ord n => Program a n -> Program a n
condenseStreams p
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


-- | Condense the reduces together
condenseReduces :: Ord n => a -> Program a n -> Program a n
condenseReduces a_fresh p
 = let (rs,xs) = go [] [] (reduces p)
   in  p { reduces   = rs
         -- For each removed reduce, prepend a postcomputation binding
         , postcomps = xs <> postcomps p }
 where
  go seen xs []
   = (seen, xs)

  -- Check if reduce is like any we've already seen
  go seen xs ((n,t):ts)
   | ((n',_):_) <- filter (reduceEquivalent t . snd) seen
   -- If so, we can just add an expression binding it.
   -- Reductions cannot refer to other reductions, so we do not need to substitute in.
   = go seen (xs <> [(n, XVar a_fresh n')]) ts

   -- This is a unique reduce so tack it on; we don't need to bind it
   | otherwise
   = go (seen <> [(n,t)]) xs ts



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


-- | Check if two reductions are equivalent
reduceEquivalent :: Ord n => Reduce a n -> Reduce a n -> Bool
reduceEquivalent r r'

 | RFold tt  ta  xk  xz  inp  <- r
 , RFold tt' ta' xk' xz' inp' <- r'
 =  tt == tt'
 && ta == ta'
 && xk `alphaEquality` xk'
 && xz `alphaEquality` xz'
 && inp == inp'

 -- Must be different constructors
 | otherwise
 = False



