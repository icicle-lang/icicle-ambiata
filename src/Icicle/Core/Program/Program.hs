-- | An entire core program
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import              P


-- | Core program composed of different stages of bindings
data Program a n =
 Program {
 -- | The type of the input/concrete feature
   inputType    :: ValType
 , inputName    :: Name n
 , snaptimeName :: Name n

 -- | All precomputations, made before starting to read from feature source
 , precomps     :: [(Name n, Exp a n)]

 -- | Stream things
 , streams      :: [Stream a n]

 -- | Postcomputations with access to last value of all streams
 , postcomps    :: [(Name n, Exp a n)]

 -- | The return values
 , returns      :: [(OutputName, Exp a n)]
 }
 deriving (Show, Eq, Ord)


renameProgram :: (Name n -> Name n') -> Program a n -> Program a n'
renameProgram f p
  = p
  { inputName   = f $ inputName p
  , snaptimeName= f $ snaptimeName p
  , precomps    = binds  renameExp      (precomps   p)
  , streams     = fmap  (renameStream f)(streams    p)
  , postcomps   = binds  renameExp      (postcomps  p)
  -- Now, we actually do not want to modify the names of the outputs.
  -- They should stay the same over the entire life of the program.
  , returns     = fmap (\(a,b) -> (a, renameExp f b)) (returns    p)
  }
  where
   binds r = fmap (\(a,b) -> (f a, r f b))



-- Pretty printing -------------

instance Pretty n => Pretty (Program a n) where
 pretty p
  =     text "Program ("
  <> pretty (inputName p) <> " : Stream " <> pretty (inputType p) <> text ", "
  <> pretty (snaptimeName p) <> " : SNAPSHOT_TIME)" <> line

  <>    text "Precomputations:"                        <> line
  <>    ppbinds (precomps p)                           <> line
  <>    text "Streams:"                                <> line
  <>    vcat (fmap pretty (streams p))                 <> line
  <>    text "Postcomputations"                        <> line
  <>    ppbinds (postcomps p)                          <> line
  <>    text "Returning:"                              <> line
  <>    ppbinds (returns   p)                          <> line

  where
   ppbinds :: (Pretty a, Pretty b) => [(a,b)] -> Doc
   ppbinds
    = vcat
    . fmap prettyNamed

   prettyNamed (nm,bind)
    = padDoc 20 (pretty nm) <> " = " <> indent 0 (pretty bind)


