-- | Describe Core expression fragment
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Exp (
      Exp
    , coreFragment
    , coreFragmentWorkerFun
    ) where

import              Icicle.Common.Fragment
import qualified    Icicle.Common.Exp.Exp   as Exp
import qualified    Icicle.Core.Exp.Prim    as Prim

import              P

type Exp n = Exp.Exp n Prim.Prim

coreFragment :: Fragment Prim.Prim
coreFragment
 = Fragment
 { typeOfPrim           = Prim.typeOfPrim
 , primsFullyApplied    = True
 , allowLambdas         = AllowLambdasAsPrimArgs
 }

coreFragmentWorkerFun :: Fragment Prim.Prim
coreFragmentWorkerFun
 = coreFragment
 { allowLambdas = AllowLambdasAsPrimArgsAndTop }
