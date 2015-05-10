-- | An entire core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Program (
      Program (..)
    , renameProgram
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp.Exp (renameExp)
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
 deriving (Show, Eq, Ord)


renameProgram :: (Name n -> Name n') -> Program n -> Program n'
renameProgram f p
  = p
  { precomps    = binds  renameExp      (precomps   p)
  , streams     = binds  renameStream   (streams    p)
  , reduces     = binds  renameReduce   (reduces    p)
  , postcomps   = binds  renameExp      (postcomps  p)
  , returns     =        renameExp f    (returns    p)
  }
  where
   binds r = fmap (\(a,b) -> (f a, r f b))


-- Pretty printing -------------

instance Pretty n => Pretty (Program n) where
 pretty p
  =     text "Program (source : Stream " <> pretty (input p) <> text ")" <> line
  <>    text "Precomputations:"                        <> line    
  <>    ppbinds (precomps p)                           <> line
  <>    text "Stream transformers:"                    <> line    
  <>    ppbinds (streams p)                            <> line
  <>    text "Reductions:"                             <> line    
  <>    ppbinds (reduces p)                            <> line
  <>    text "Postcomputations:"                       <> line    
  <>    ppbinds (postcomps p)                          <> line
  <>    text "Returning:"                              <> line    
  <>    indent 4 (pretty  $ returns   p)               <> line

  where
   ppbinds :: (Pretty a, Pretty b) => [(a,b)] -> Doc
   ppbinds
    = vcat
    . fmap (\(a,b) -> pretty a <+> text "=" <> line <> indent 4 (pretty b))
