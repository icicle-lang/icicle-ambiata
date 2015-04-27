-- A whole core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Program (
      Program (..)
    ) where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp
import              Icicle.Core.Stream
import              Icicle.Core.Reduce

-- import              P

data Program n =
 Program
 { input        :: ValType
 , precomps     :: [(Name n, Exp n)]
 , streams      :: [(Name n, Stream n)]
 , reduces      :: [(Name n, Reduce n)]
 , postcomps    :: [(Name n, Exp n)]
 , returns      :: Exp n
 }

