-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , pullLets
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => Program n p -> Fresh n (Program n p)
simpAvalanche p
 = pullLets <$> transformX return simp p


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

    Update n x
     -> pres x (Update n)
    Push n x
     -> pres x (Push n)

 where
  pres x instmt
   = let (bs, x') = takeLets x
     in  foldr mkLet (instmt x') bs

  mkLet (n,x) s
   = Let n x [s]

  go = fmap pullStmt
