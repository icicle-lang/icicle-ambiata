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
import Icicle.Common.Type
import Icicle.Common.Exp hiding (Exp)
import Icicle.Core.Exp
import Icicle.Core.Program.Program
import Icicle.Core.Stream.Stream
import Icicle.Core.Program.Subst

import P

-- | Condense streams then reductions
condenseProgram :: Ord n => a -> Program a n -> Program a n
condenseProgram a_fresh p
 = let pre'  = condensePrecomps  a_fresh p
       strm' = condenseStreams   a_fresh pre'
       post' = condensePostcomps a_fresh strm'
   in  post'

condensePrecomps :: Ord n => a -> Program a n -> Program a n
condensePrecomps a_fresh p
 = let (sub, pres') = condenseBinds a_fresh (precomps p)
   in  p { precomps  = pres'
         , streams   = unsafeSubstStreams  sub (streams   p)
         , postcomps = unsafeSubstSnds     sub (postcomps p)
         , returns   = unsafeSubstSnds     sub (returns   p)
         }

condensePostcomps :: Ord n => a -> Program a n -> Program a n
condensePostcomps a_fresh p
 = let (sub, post') = condenseBinds a_fresh (postcomps p)
   in  p { postcomps = post'
         , returns   = unsafeSubstSnds     sub (returns   p)
         }

condenseBinds :: Ord n => a -> [(Name n, Exp a n)] -> ([(Name n, Name n)], [(Name n, Exp a n)])
condenseBinds _ binds
 = go [] binds
 where
  go seen []
   = ([], seen)
  go seen ((n,x):bs)

   | ((n',_):_) <- filter (alphaEquality x . snd) seen
   , sub        <- [(n, n')]
   = (sub, []) <> go seen (unsafeSubstSnds sub bs)

   | otherwise
   = go (seen <> [(n,x)]) bs

-- | Condense the stream operations together
condenseStreams :: Ord n => a -> Program a n -> Program a n
condenseStreams a_fresh p
 = let (ss, sub) = go [] (streams p) []
   in  p { streams = ss
         , postcomps = unsafeSubstSnds sub (postcomps p)
         , returns   = unsafeSubstSnds sub (returns p) }
 where
  go seen [] sub
   = (seen, sub)
  go seen (t:ts) sub
   = let (left,sub')= tryInsert a_fresh seen t
         seen'      = seen <> maybeToList left
         ts'        = unsafeSubstStreams sub' ts
     in go seen' ts' (sub <> sub')


-- | Try to insert as much of this stream into the "seen" ones.
-- Returns any remaining part of the insertion stream,
-- and successful insertions as a substitution list
tryInsert
        :: Ord n
        => a
        -> [Stream a n]
        -> Stream a n
        -> (Maybe (Stream a n), [(Name n, Name n)])
tryInsert a_fresh seen t
 -- Check if fold is like any we've already seen
 |  SFold n  _ _ _          <- t
 , (SFold n' _ _ _ : _)     <- similar
 -- If so, whole stream can be replaced by substituting n := n'.
 = (Nothing, [(n, n')])

 -- For filters we need to insert each substream individually
 | SFilter x ss             <- t
 = let filts      = concatMap substreams similar
       (ss',sub') = goFilter filts ss

       f'         | null ss'
                  = Nothing
                  | otherwise
                  = Just (SFilter x ss')
   in  (f', sub')

 -- Cannot insert anything
 | otherwise
 = (Just t, [])

 where
  similar = filter (streamSimilar a_fresh t) seen
  substreams (SFilter _ ss) = ss
  substreams (SFold{})      = []

  goFilter _ []
   = ([], [])
  goFilter against (s:ss)
   = let (left,sub) = tryInsert a_fresh against s
         left'      = maybeToList left
         against'   = against <> left'
         ss'        = unsafeSubstStreams sub ss
     in (left',sub) <> goFilter against' ss'


-- | Check if two streams are equivalent fold, or filter with same predicate.
-- For filter, ignore streams.
streamSimilar :: Ord n => a -> Stream a n -> Stream a n -> Bool
streamSimilar a_fresh s s'

 | SFold n  st  z  k     <- s
 , SFold n' st' z' k'    <- s'
 =  st  == st'
 && z `alphaEquality` z'
 && wrap n k `alphaEquality` wrap n' k'

 -- Not necessarily checking equivalence, just equivalence of context
 | SFilter x  _         <- s
 , SFilter x' _         <- s'
 =  x `alphaEquality` x'

 -- Must be different constructors
 | otherwise
 = False

 where
  -- Magic wrapper - put a lambda around both so that
  -- the names are considered equivalent
  wrap n x
   = XLam a_fresh n UnitT x


