-- Naive evaluation
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}

module Icicle.Sketch.Eval where
import Icicle.Sketch.Virtual


lookbehind :: Program x t u -> Int
lookbehind p
 = case p of
    In _
     -> 0
    LetS _ p'
     -> lookbehind p'
    LetR (Latest i _ _) p'
     -> i `max` lookbehind p'


evalTopLevel :: TopLevel x u -> [x] -> u
evalTopLevel p xs
 = evalProgram p xs ()


evalProgram :: Program x t u -> [x] -> t -> u
evalProgram p vals inp
 = case p of
    In s
     -> s inp
    LetS s p'
     -> evalProgram p' vals (s inp, inp)
    LetR (Latest i ts r) p'
     -> let latest = take i vals
            trans  = evalTransforms ts latest
            red    = evalReduce     r  trans
        in  evalProgram p' vals (red, inp)


evalTransforms :: Transforms t u -> [t] -> [u]
evalTransforms ts vs
 = case ts of
    End
     -> vs
    Chain t ts'
     -> let vs' = evalTransform t vs
        in  evalTransforms ts' vs'


evalTransform :: Transform t u -> [t] -> [u]
evalTransform t vs
 = case t of
    Map f
     -> map f vs
    Filter p
     -> filter p vs


evalReduce :: Reduce t u -> [t] -> u
evalReduce (Reduce z k) vs
 = foldl (curry k) (z ()) vs

