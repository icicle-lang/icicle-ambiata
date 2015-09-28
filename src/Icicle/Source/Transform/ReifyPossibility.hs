{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Transform.ReifyPossibility (
    reifyPossibilityTransform
  ) where

import Icicle.Source.Query
import Icicle.Source.Type
import Icicle.Source.Transform.Base

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import              Data.Functor.Identity
import              Data.List (zip)
import qualified    Data.Map as Map

reifyPossibilityTransform
        :: Ord n
        => Transform (Fresh n) () (Annot a n) n
reifyPossibilityTransform
 = Transform
 { transformExp         = tranx
 , transformPat         = \_ p -> return ((), p)
 , transformContext     = tranc
 , transformState       = ()
 }
 where
  tranx _ x
   = return ((), x)
  tranc _ c
   = return ((), c)

