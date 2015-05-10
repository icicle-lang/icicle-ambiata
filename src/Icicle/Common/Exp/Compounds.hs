-- | Some useful things to do with expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Compounds (
      takeApps
    , takePrimApps
    ) where

import              Icicle.Common.Exp.Exp
import              P


-- | Split an expression into its function part and any arguments applied to it.
-- If it's not an application, arguments will be empty.
takeApps :: Exp n p -> (Exp n p, [Exp n p]) 
takeApps xx
 = case xx of
    XApp p q
     -> let (f,as) = takeApps p
        in  (f, as <> [q])
    _
     -> (xx, [])


-- | Check if an expression is a primitive application
takePrimApps :: Exp n p -> Maybe (p, [Exp n p])
takePrimApps xx
 = case takeApps xx of
    (XPrim p, args) -> Just (p, args)
    _               -> Nothing


