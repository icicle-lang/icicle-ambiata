{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Base (
    CoreBinds    (..)
  , ConvertError (..)
  , ConvertM
  , pre, strm, red, post
  , programOfBinds
  , freshly
  ) where

import qualified        Icicle.Core             as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type
import qualified        Icicle.Common.Exp       as X

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  P

import                  Data.String (String)

data CoreBinds n
 = CoreBinds
 { precomps     :: [(Name n, C.Exp n)]
 , streams      :: [(Name n, C.Stream n)]
 , reduces      :: [(Name n, C.Reduce n)]
 , postcomps    :: [(Name n, C.Exp n)]
 }

programOfBinds
    :: ValType
    -> CoreBinds n
    -> Name n
    -> C.Program n
programOfBinds inpType binds ret
 = C.Program
 { C.input      = inpType
 , C.precomps   = precomps  binds
 , C.streams    = streams   binds
 , C.reduces    = reduces   binds
 , C.postcomps  = postcomps binds
 , C.postdate   = Nothing
 , C.returns    = X.XVar ret
 }

instance Monoid (CoreBinds n) where
 mempty = CoreBinds [] [] [] []
 mappend (CoreBinds a b c d) (CoreBinds e f g h)
  = CoreBinds (a<>e) (b<>f) (c<>g) (d<>h)


pre :: Name n -> C.Exp n -> CoreBinds n
pre n x = mempty { precomps = [(n,x)] }

strm :: Name n -> C.Stream n -> CoreBinds n
strm n x = mempty { streams = [(n,x)] }

red :: Name n -> C.Reduce n -> CoreBinds n
red n x = mempty { reduces = [(n,x)] }

post :: Name n -> C.Exp n -> CoreBinds n
post n x = mempty { postcomps = [(n,x)] }



data ConvertError a n
 = ConvertErrorTODO a String
 | ConvertErrorPrimAggregateNotAllowedHere a Agg
 | ConvertErrorPrimNoArguments a Int Prim
 | ConvertErrorGroupByHasNonGroupResult a UniverseType
 | ConvertErrorExpNoSuchVariable a n
 | ConvertErrorExpNestedQueryNotAllowedHere a (Query (a,UniverseType) n)
 | ConvertErrorExpApplicationOfNonPrimitive a (Exp (a,UniverseType) n)
 deriving (Show, Eq, Ord)


type ConvertM a n r
 = FreshT n (Either (ConvertError a n)) r

freshly :: (Name n -> r) -> ConvertM a n (r, Name n)
freshly f
 = do   n' <- fresh
        return (f n', n')

