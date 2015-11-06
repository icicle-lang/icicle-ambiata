{-# LANGUAGE NoImplicitPrelude #-}

-- | Annotation fiddles for Avalanche programs.
module Icicle.Avalanche.Annot
  ( eraseAnnotP, eraseAnnotX, eraseAnnotS
  , reannotP, reannotX, reannotS
  ) where

import Icicle.Avalanche.Program
import Icicle.Avalanche.Statement.Statement
import Icicle.Common.Exp

import P


eraseAnnotP :: Program a n p -> Program () n p
eraseAnnotP = reannotP (const ())

eraseAnnotS :: Statement a n p -> Statement () n p
eraseAnnotS = reannotS (const ())

eraseAnnotX :: Exp a n p -> Exp () n p
eraseAnnotX = reannotX (const ())

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

    ForeachInts n x1 x2 s
     -> ForeachInts n (reannotX f x1) (reannotX f x2) (reannotS f s)

    ForeachFacts fs t ft s
     -> ForeachFacts fs t ft (reannotS f s)

    Block s
     -> Block (fmap (reannotS f) s)

    InitAccumulator a s
     -> InitAccumulator (reannotA f a) (reannotS f s)

    Read n1 n2 vt s
     -> Read n1 n2 vt (reannotS f s)

    Write n x
     -> Write n (reannotX f x)

    Push n x
     -> Push n (reannotX  f x)

    Output n vt xs
     -> Output n vt (fmap (first (reannotX f)) xs)

    KeepFactInHistory  -> KeepFactInHistory
    LoadResumable n vt -> LoadResumable n vt
    SaveResumable n vt -> SaveResumable n vt


reannotX :: (a -> a') -> Exp a n p -> Exp a' n  p
reannotX f xx
 = case xx of
    XVar    a n     -> XVar   (f a) n
    XPrim   a p     -> XPrim  (f a) p
    XValue  a t v   -> XValue (f a) t v
    XApp    a x1 x2 -> XApp   (f a) (reannotX f x1) (reannotX f x2)
    XLam    a n t x -> XLam   (f a) n t (reannotX f x)
    XLet    a n b x -> XLet   (f a) n (reannotX f b) (reannotX f x)


reannotA :: (a -> a') -> Accumulator a n p -> Accumulator a' n p
reannotA f aa
 = aa { accInit = reannotX f (accInit aa) }
