{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Simp
     ( simpProgram
     ) where


import Icicle.Common.Fresh
import qualified Icicle.Common.Exp.Simp.Beta as B
import Icicle.Core.Program.Program
import Icicle.Core.Stream.Stream
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
       ss   <- mapM (simpStream a_fresh) (streams   p)
       rets <- forall simp       (returns   p)
       return p { precomps  = pres
                , streams   = ss
                , postcomps = poss
                , returns   = rets }
  where forall f = sequenceA . fmap (sequenceA . fmap (f a_fresh))


-- | Simp the exps in stream
--
simpStream :: Ord n => a -> Stream a n -> Fresh n (Stream a n)
simpStream a_fresh ss
 = case ss of
  SFold n t z k
   -> SFold n t <$> simp a_fresh z <*> simp a_fresh k
  SFilter x ss'
   -> SFilter <$> simp a_fresh x <*> mapM (simpStream a_fresh) ss'

