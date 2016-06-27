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
  = InlineUsingLets
  | InlineUsingSubst

defaultInline :: InlineOption
defaultInline = InlineUsingLets

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
      InlineUsingLets -> do
       ns      <- mapM (freshPrefixBase . nameBase) argNames

       let lets = fmap (mkLet a) (ns `zip` args)
       let sub  = Map.fromList
                $ argNames `zip` fmap (Var a) ns

       body'   <- substQ sub $ body fun

       return ((), Nested a (prefixContexts lets body'))

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

  mkLet a (n,d) = Let a n d

  prefixContexts cs (Query cs' xx)
   = Query (cs <> cs') xx


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

