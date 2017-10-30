{-# LANGUAGE NoImplicitPrelude #-}

-- | Annotation fiddles for Avalanche programs.
module Icicle.Avalanche.Annot
  ( eraseAnnotP, eraseAnnotX, eraseAnnotS
  , reannotP, reannotX, reannotS
  ) where

import Icicle.Avalanche.Program
import Icicle.Avalanche.Statement.Statement
-- Re-exported as well as used
import Icicle.Common.Exp.Compounds (reannotX, eraseAnnotX)

import P


eraseAnnotP :: Program a n p -> Program () n p
eraseAnnotP = reannotP (const ())

eraseAnnotS :: Statement a n p -> Statement () n p
eraseAnnotS = reannotS (const ())

reannotP :: (a -> a') -> Program a n p -> Program a' n p
reannotP f p
 = p { statements = reannotS f (statements p) }

reannotS :: (a -> a') -> Statement a n p -> Statement a' n p
reannotS f ss
 = case ss of
    If x s1 s2
     -> If (reannotX f x) (reannotS f s1) (reannotS f s2)

    Let n x s
     -> Let n (reannotX f x) (reannotS f s)

    While t n nt x2 s
     -> While t n nt (reannotX f x2) (reannotS f s)

    ForeachInts t n x1 x2 s
     -> ForeachInts t n (reannotX f x1) (reannotX f x2) (reannotS f s)

    ForeachFacts binds t s
     -> ForeachFacts binds t (reannotS f s)

    Block s
     -> Block (fmap (reannotS f) s)

    InitAccumulator a s
     -> InitAccumulator (reannotA f a) (reannotS f s)

    Read n1 n2 vt s
     -> Read n1 n2 vt (reannotS f s)

    Write n x
     -> Write n (reannotX f x)

    Output n vt xs
     -> Output n vt (fmap (first (reannotX f)) xs)


reannotA :: (a -> a') -> Accumulator a n p -> Accumulator a' n p
reannotA f aa
 = aa { accInit = reannotX f (accInit aa) }
