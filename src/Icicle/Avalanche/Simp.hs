-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , pullLets
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => Program n p -> Fresh n (Program n p)
simpAvalanche p
 = do p' <- transformX return simp p
      s' <- forwardStmts $ pullLets $ statements p'
      return $ p { statements = s' }


pullLets :: Statement n p -> Statement n p
pullLets stm
 = case stm of
    If x subs elses
     -> pres x (\x' -> If x' (pullLets subs) (pullLets elses))
    Let n x subs
     -> pres x (\x' -> Let n x' $ pullLets subs)

    ForeachInts n from to subs
     -> pres from
     $ \from'
     -> pres to
     $ \to'
     -> ForeachInts n from' to' $ pullLets subs

    ForeachFacts n ty subs
     -> ForeachFacts n ty $ pullLets subs
     
    Block subs
     -> Block (fmap pullLets subs)

    InitAccumulator (Accumulator n at vt x) subs
     -> pres x (\x' -> InitAccumulator (Accumulator n at vt x') $ pullLets subs)

    Read n acc subs
     -> Read n acc (pullLets subs)

    Write n x
     -> pres x (Write n)
    Push n x
     -> pres x (Push n)

    Return x
     -> pres x (\x' -> Return x')

 where
  pres x instmt
   = let (bs, x') = takeLets x
     in  foldr mkLet (instmt x') bs

  mkLet (n,x) s
   = Let n x s


-- | Let-forwarding on statements
forwardStmts :: Ord n => Statement n p -> Fresh n (Statement n p)
forwardStmts s
 = case s of
    If x ss es
     -> If x    <$> go ss <*> go es

    Let n x ss
     | isSimpleValue x
     -> substXinS n x ss

     | otherwise
     -> Let n x <$> go ss

    ForeachInts n from to ss
     -> ForeachInts n from to <$> go ss

    ForeachFacts n ty ss
     -> ForeachFacts n ty <$> go ss

    Block ss
     -> Block <$> mapM go ss

    InitAccumulator acc ss
     -> InitAccumulator acc <$> go ss

    Read n acc ss
     -> Read n acc <$> go ss 

    Write n x
     -> return $ Write n x
    Push  n x
     -> return $ Push  n x

    Return  x
     -> return $ Return  x

 where
  go = forwardStmts


substXinS :: Ord n => Name n -> Exp n p -> Statement n p -> Fresh n (Statement n p)
substXinS name payload s
 = case s of
    If x ss es
     -> If <$> sub x <*> go ss <*> go es
    Let n x ss
     -- TODO name avoiding grr
     -> Let n <$> sub x <*> go ss

    ForeachInts n from to ss
     -> ForeachInts n <$> sub from <*> sub to <*> go ss

    ForeachFacts n ty ss
     -> ForeachFacts n ty <$> go ss

    Block ss
     -> Block <$> mapM go ss

    InitAccumulator (Accumulator n at vt x) ss
     -> InitAccumulator <$> (Accumulator n at vt <$> sub x) <*> go ss

    Read n acc ss
     -> Read n acc <$> go ss

    Write n x
     -> Write n <$> sub x
    Push  n x
     -> Push  n <$> sub x

    Return  x
     -> Return  <$> sub x

 where
  sub = subst     name payload
  go  = substXinS name payload
