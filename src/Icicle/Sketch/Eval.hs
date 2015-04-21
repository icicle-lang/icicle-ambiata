{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}

module Icicle.Sketch.Eval where
import Icicle.Sketch.Virtual

evalVirtual :: Virtual (Stream t) u -> [ValueOf t] -> ValueOf u
evalVirtual (TakeN i ts r) vals
 = let latest = take i vals
       trans  = evalTransforms ts latest
       red    = evalReduce     r  trans
   in  red

evalTransforms :: Transforms t u -> [ValueOf t] -> [ValueOf u]
evalTransforms ts vs
 = case ts of
    End
     -> vs
    Chain t ts'
     -> let vs' = evalTransform t vs
        in  evalTransforms ts' vs'

evalTransform :: Transform t u -> [ValueOf t] -> [ValueOf u]
evalTransform t vs
 = case t of
    Map f
     -> map f vs
    Filter p
     -> filter p vs

evalReduce :: Reduce t u -> [ValueOf t] -> ValueOf u
evalReduce (Reduce z k) vs
 = foldl (curry k) (z ()) vs

