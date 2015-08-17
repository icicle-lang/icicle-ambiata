{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.Inline (
    inlineTransform
  , inlineQT
  , inlineQ
  ) where

import Icicle.Source.Query
import Icicle.Source.Transform.Base
import Icicle.Source.Transform.SubstX

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import              Data.Functor.Identity
import              Data.List (zip)
import qualified    Data.Map as Map



inlineTransform
        :: Ord n
        => Map.Map (Name n) (Function a n)
        -> Transform (Fresh n) () a n
inlineTransform funs
 = Transform
 { transformExp     = tranx
 , transformContext = tranc
 , transformState   = ()
 }
 where
  tranx _ x
   | (Var a n, args) <- takeApps x
   , Just fun        <- Map.lookup n funs
   , length args == length (arguments fun)
   = do let argNames
                    = fmap snd $ arguments fun
        ns         <- mapM freshPrefix' argNames

        let sub     = Map.fromList
                    $ argNames `zip` fmap (Var a) ns
        let sub'    = unsafeSubstTransform
                    { transformState = sub }

        let body'   = runIdentity
                    $ transformQ sub'
                    $ body fun

        let lets = fmap (mkLet a) (ns `zip` args)
        return ((), Nested a (prefixContexts lets body'))

   | otherwise
   = return ((), x)

  tranc _ c
   = return ((), c)

  mkLet a (n,d) = Let a n d

  prefixContexts cs (Query cs' xx)
   = Query (cs <> cs') xx


inlineQT :: Ord n
        => Map.Map (Name n) (Function a n)
        -> QueryTop a n
        -> Fresh n (QueryTop a n)
inlineQT funs
 = transformQT (inlineTransform funs)


inlineQ :: Ord n
        => Map.Map (Name n) (Function a n)
        -> Query a n
        -> Fresh n (Query a n)
inlineQ funs
 = transformQ (inlineTransform funs)
