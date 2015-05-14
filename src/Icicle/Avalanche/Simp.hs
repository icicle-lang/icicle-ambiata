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
 $ fmap pullStmt stms

pullStmt :: Statement n p -> Statement n p
pullStmt stm
 = case stm of
    If x subs
     -> pres x (\x' -> If x'    $ go subs)
    Let n x subs
     -> pres x (\x' -> Let n x' $ go subs)

    Read n acc subs
     -> Read n acc (go subs)

    Block subs
     -> Block (go subs)

    Write n x
     -> pres x (Write n)
    Push n x
     -> pres x (Push n)

 where
  pres x instmt
   = let (bs, x') = takeLets x
     in  foldr mkLet (instmt x') bs

  mkLet (n,x) s
   = Let n x [s]

  go = fmap pullStmt



forwardLoop :: Ord n => FactLoop n p -> Fresh n (FactLoop n p)
forwardLoop (FactLoop v n ss)
 = FactLoop v n <$> mapM forwardStmts ss

-- | Let-forwarding on statements
forwardStmts :: Ord n => Statement n p -> Fresh n (Statement n p)
forwardStmts s
 = case s of
    If x ss
     -> If x    <$> go ss

    Let n x ss
     | isSimpleValue x
     -> do  ss'    <- mapM (substXinS n x) ss
            Block <$> go ss'

     | otherwise
     -> Let n x <$> go ss

    Read n acc ss
     -> Read n acc <$> go ss 

    Block ss
     -> Block <$> go ss

    Write n x
     -> return $ Write n x
    Push n x
     -> return $ Push  n x

 where
  go = mapM forwardStmts


substXinS :: Ord n => Name n -> Exp n p -> Statement n p -> Fresh n (Statement n p)
substXinS name payload s
 = case s of
    If x ss
     -> If <$> sub x <*> go ss
    Let n x ss
     -- TODO name avoiding grr
     -> Let n <$> sub x <*> go ss
    Block ss
     -> Block <$> go ss

    Read n acc ss
     -> Read n acc <$> go ss

    Write n x
     -> Write n <$> sub x
    Push  n x
     -> Push  n <$> sub x

 where
  sub = subst     name payload
  go  = mapM (substXinS name payload)
