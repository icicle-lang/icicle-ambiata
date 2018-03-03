{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.Inline (
    inlineTransform
  , inlineQT
  , inlineQ
  , InlineOption (..)
  , defaultInline
  ) where

import Icicle.Source.Query
import Icicle.Source.Transform.Base
import Icicle.Source.Transform.SubstX

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import              Data.List (zip)
import qualified    Data.Map as Map
import              Data.Hashable (Hashable)


data InlineOption
  = InlineUsingSubst

defaultInline :: InlineOption
defaultInline = InlineUsingSubst

inlineTransform
        :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Function a n)
        -> Transform (Fresh n) () a n
inlineTransform opt funs
 = Transform
 { transformExp     = tranx
 , transformPat     = tranp
 , transformContext = tranc
 , transformState   = ()
 }
 where
  tranx _ x
   | (Var a n, args) <- takeApps x
   , Just fun        <- Map.lookup n funs
   , vars            <- arguments fun
   , argNames        <- fmap snd vars
   , length args == length vars
   = case opt of
      InlineUsingSubst -> do
       let sub  = Map.fromList
                $ argNames `zip` args
       body'   <- substQ sub $ body fun

       return ((), Nested a body')

   | otherwise
   = return ((), x)

  tranp _ p
   = return ((), p)

  tranc _ c
   = return ((), c)

inlineQT :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Function a n)
        -> QueryTop a n
        -> Fresh n (QueryTop a n)
inlineQT opt funs qt
 = simplifyNestedQT <$> transformQT (inlineTransform opt funs) qt


inlineQ :: (Hashable n, Eq n)
        => InlineOption
        -> Map.Map (Name n) (Function a n)
        -> Query a n
        -> Fresh n (Query a n)
inlineQ opt funs
 = transformQ (inlineTransform opt funs)

