-- Some example virtual features
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}

module Icicle.Sketch.Egs where
import Icicle.Sketch.Virtual

fold :: (b -> a -> b) -> b -> Latest a b
fold k z = Latest 60 End
         $ Reduce (const z) (uncurry k)

minmax :: TopLevel Int (Int,Int)
minmax
 = LetR (fold min 1000000)
 $ LetR (fold max 0)
 $ In (\(lo,(hi,())) -> (lo,hi))

