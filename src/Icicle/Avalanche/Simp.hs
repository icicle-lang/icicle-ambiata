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
 = do p' <- pullLets <$> transformX return simp p
      l' <- forwardLoop (loop p')
      return $ p { loop = l' }


pullLets :: Program n p -> Program n p
pullLets p
 = p
 { precomps = pullExps $ precomps p
 , loop     = pullLoop $ loop p
 , postcomps= pullExps $ postcomps p
 }

pullExps :: [(Name n, Exp n p)] -> [(Name n, Exp n p)]
pullExps
 = concatMap pullX
 where
  pullX (n,x)
   = let (bs,x') = takeLets x
     in  bs <> [(n,x')]

pullLoop :: FactLoop n p -> FactLoop n p
pullLoop (FactLoop v n stms)
 = FactLoop v n
 $ pullStmt stms

pullStmt :: Statement n p -> Statement n p
pullStmt stm
 = case stm of
    If x subs elses
     -> pres x (\x' -> If x' (go subs) (go elses))
    Let n x subs
     -> pres x (\x' -> Let n x' $ go subs)

    -- TODO: does this require renaming?
    Foreach n from to subs
     -> pres from
     $ \from'
     -> pres to
     $ \to'
     -> Foreach n from' to' $ go subs
     
    Block subs
     -> Block (fmap go subs)

    Read n acc subs
     -> Read n acc (go subs)

    Write n x
     -> pres x (Write n)
    Push n x
     -> pres x (Push n)

 where
  pres x instmt
   = let (bs, x') = takeLets x
     in  foldr mkLet (instmt x') bs

  mkLet (n,x) s
   = Let n x s

  go = pullStmt



forwardLoop :: Ord n => FactLoop n p -> Fresh n (FactLoop n p)
forwardLoop (FactLoop v n ss)
 = FactLoop v n <$> forwardStmts ss

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

    Foreach n from to ss
     -> Foreach n from to <$> go ss

    Block ss
     -> Block <$> mapM go ss

    Read n acc ss
     -> Read n acc <$> go ss 

    Write n x
     -> return $ Write n x
    Push n x
     -> return $ Push  n x

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

    Foreach n from to ss
     -> Foreach n <$> sub from <*> sub to <*> go ss

    Block ss
     -> Block <$> mapM go ss

    Read n acc ss
     -> Read n acc <$> go ss

    Write n x
     -> Write n <$> sub x
    Push  n x
     -> Push  n <$> sub x

 where
  sub = subst     name payload
  go  = substXinS name payload
