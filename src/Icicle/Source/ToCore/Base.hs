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

import qualified        Icicle.Core as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type
import                  Icicle.Common.Exp

import                  P

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
 , C.returns    = XVar ret
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



data ConvertError n
 = ConvertErrorTODO
 deriving (Show, Eq, Ord)

type ConvertM n a
 = FreshT n (Either (ConvertError n)) a

freshly :: (Name n -> a) -> ConvertM n (a, Name n)
freshly f
 = do   n' <- fresh
        return (f n', n')

