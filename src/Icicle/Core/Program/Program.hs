-- | An entire core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Program (
      Program (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp
import              Icicle.Core.Stream.Stream
import              Icicle.Core.Reduce.Reduce

import              P


-- | Core program composed of different stages of bindings
data Program n =
 Program
 -- | The type of the input/concrete feature
 { input        :: ValType

 -- | All precomputations, made before starting to read from feature source
 , precomps     :: [(Name n, Exp n)]

 -- | Stream transformers that work on feature source
 , streams      :: [(Name n, Stream n)]

 -- | Reductions over streams.
 -- There can be no dependencies between these:
 -- a fold worker function cannot mention the result of another fold,
 -- as that would require two passes!
 , reduces      :: [(Name n, Reduce n)]

 -- | Postcomputations with access to precomputations and reduces
 , postcomps    :: [(Name n, Exp n)]

 -- | The single return value
 , returns      :: Exp n
 }


-- Pretty printing -------------

instance Pretty n => Pretty (Program n) where
 pretty p
  =     text "Program " <> brackets (pretty $ input p) <> line
  <>    text "PRE"                                     <> line    
  <>    ppbinds (precomps p)                           <> line
  <>    text "STR"                                     <> line    
  <>    ppbinds (streams p)                            <> line
  <>    text "RED"                                     <> line    
  <>    ppbinds (reduces p)                            <> line
  <>    text "POST"                                    <> line    
  <>    ppbinds (postcomps p)                          <> line
  <>    text "IN"                                      <> line    
  <>    pretty  (returns   p)                          <> line

  where
   ppbinds :: (Pretty a, Pretty b) => [(a,b)] -> Doc
   ppbinds
    = vcat
    . fmap (\(a,b) -> pretty a <+> indent 8 (text "=" <+> pretty b))
