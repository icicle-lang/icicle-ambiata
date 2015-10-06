{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Simp
     ( simpProgram
     ) where


import Icicle.Common.Fresh
import qualified Icicle.Common.Exp.Simp.Beta as B
import Icicle.Core.Program.Program
import Icicle.Core.Stream.Stream
import Icicle.Core.Reduce.Reduce
import qualified Icicle.Core.Exp.Simp as S
import qualified Icicle.Core.Exp.Exp as C

import P


simp :: Ord n => a -> C.Exp a n -> Fresh n (C.Exp a n)
simp a_fresh = S.simp a_fresh B.isSimpleValue

-- | Simplifies individual exps in the Core prorgam.
--
--   note: I don't know how this should handle bindings
--         especially across across stages...
--         say precomps have: x = 1, y = 2, z = x + y
--         we probably don't want to get rid of the names x and y
--         but maybe we do want to simplify z?
--         --tranma
--
simpProgram :: Ord n => a -> Program a n -> Fresh n (Program a n)
simpProgram a_fresh p
  = do pres <- forall simp       (precomps  p)
       poss <- forall simp       (postcomps p)
       ss   <- forall simpStream (streams   p)
       rs   <- forall simpReduce (reduces   p)
       rets <- forall simp       (returns   p)
       return p { precomps  = pres
                , streams   = ss
                , reduces   = rs
                , postcomps = poss
                , returns   = rets }
  where forall f = sequenceA . fmap (sequenceA . fmap (f a_fresh))


-- | Simp the exps in stream
--
simpStream :: Ord n => a -> Stream a n -> Fresh n (Stream a n)
simpStream a_fresh ss = case ss of
  Source
    ->    return ss
  SWindow t x mx n
    -> return (SWindow t x mx n)
  STrans t x n
    -> do x'  <- simp a_fresh x
          return (STrans t x' n)


-- | Simp the exps in reduce, perhaps we can do something better
--
simpReduce :: Ord n => a -> Reduce a n -> Fresh n (Reduce a n)
simpReduce a_fresh ss = case ss of
  RFold t1 t2 x1 x2 n
    -> do y1 <- simp a_fresh x1
          y2 <- simp a_fresh x2
          return $ RFold t1 t2 y1 y2 n
  RLatest t x n
    -> do x' <- simp a_fresh x
          return $ RLatest t x' n
