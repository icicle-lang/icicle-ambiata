{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Base (
    CoreBinds    (..)
  , ConvertError (..)
  , ConvertM
  , pre, stream, reduce, post
  ) where

import qualified        Icicle.Core as C
import                  Icicle.Common.Fresh

data CoreBinds n
 = CoreBinds
 { precomps     :: [(Name n, Exp n)]
 , streams      :: [(Name n, Stream n)]
 , reduces      :: [(Name n, Reduce n)]
 , postcomps    :: [(Name n, Exp n)]
 }

instance Monoid (CoreBinds n) where
 mempty = CoreBinds [] [] [] []
 mappend (CoreBinds a b c d) (CoreBinds e f g h)
  = CoreBinds (a<>e) (b<>f) (c<>g) (d<>h)


pre :: Name n -> Exp n -> CoreBinds n
pre n x = mempty { precomps = [(n,x)] }

stream :: Name n -> Stream n -> CoreBinds n
stream n x = mempty { streams = [(n,x)] }

reduce :: Name n -> Reduce n -> CoreBinds n
reduce n x = mempty { reduces = [(n,x)] }

post :: Name n -> Exp n -> CoreBinds n
post n x = mempty { postcomps = [(n,x)] }



data ConvertError n
 = ConvertErrorTODO
 deriving (Show, Eq, Ord)

type ConvertM n a
 = FreshT n (Either (ConvertError n)) a


