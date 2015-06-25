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


simp :: Ord n => C.Exp n -> Fresh n (C.Exp n)
simp = S.simp B.isSimpleValue

-- | Simplifies individual exps in the Core prorgam.
--
--   note: I don't know how this should handle bindings
--         especially across across stages...
--         say precomps have: x = 1, y = 2, z = x + y
--         we probably don't want to get rid of the names x and y
--         but maybe we do want to simplify z?
--         --tranma
--
simpProgram :: Ord n => Program n -> Fresh n (Program n)
simpProgram p
  = do pres <- forall simp (precomps p)
       poss <- forall simp (postcomps p)
       ss   <- forall simpStream (streams p)
       rs   <- forall simpReduce (reduces p)
       return p { precomps  = pres
                , streams   = ss
                , reduces   = rs
                , postcomps = poss}
  where forall f = sequenceA . fmap (sequenceA . fmap f)


-- | Simp the exps in stream
--
simpStream :: Ord n => Stream n -> Fresh n (Stream n)
simpStream ss = case ss of
  Source
    ->    return ss
  STrans t x1 n
    -> do x2 <- simp x1
          return (STrans t x2 n)


-- | Simp the exps in reduce, perhaps we can do something better
--
simpReduce :: Ord n => Reduce n -> Fresh n (Reduce n)
simpReduce ss = case ss of
  RFold t1 t2 x1 x2 n
    -> do y1 <- simp x1
          y2 <- simp x2
          return $ RFold t1 t2 y1 y2 n
  RLatest t x n
    -> do x' <- simp x
          return $ RLatest t x' n
