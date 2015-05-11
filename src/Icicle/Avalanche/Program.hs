-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  , Accumulator     (..)
  , AccumulatorType (..)
  , Loop            (..)
  , Statement       (..)
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              Icicle.Internal.Pretty

import              P

-- | An entire Avalanche program
data Program n p =
  Program
  { precomps    :: [(Name n, Exp n p)]
  , accums      :: [Accumulator n p]
  , loop        :: Loop n p
  , postcomps   :: [(Name n, Exp n p)]
  , returns     :: Exp n p
  }
 deriving (Eq, Ord, Show)


-- | Mutable accumulators
data Accumulator n p
 = Accumulator (Name n)
               (AccumulatorType n p)
 deriving (Eq, Ord, Show)


-- | There are three different kinds of reductions,
-- so three different kinds of accumulators.
data AccumulatorType n p
 -- | Resumable folds, where we store the value for next time
 --
 -- Exp is initial value - only if no history.
 = Resumable ValType (Exp n p)
 -- | Windowed but not latest folds, where for each update we mark
 -- the current fact as necessary for next time
 --
 -- Exp is initial value.
 | Windowed  ValType (Exp n p)
 -- | Latest N, where the value is not so much updated as a
 -- fact is pushed on
 --
 -- Exp is size/count.
 | Latest    ValType (Exp n p)
 deriving (Eq, Ord, Show)


-- | A streaming loop over the inputs.
-- This will be run for every marked value from last execution, plus the new values.
data Loop n p =
  Loop  ValType -- ^ What are we iterating over?
        [Statement n p]
 deriving (Eq, Ord, Show)

-- | Part of a loop
data Statement n p
 -- Branches
 -- | An IF for filters
 = If (Exp n p)                 [Statement n p]
 -- | An if for windows
 | IfWindowed Int               [Statement n p]
 -- | Local binding, so the name better be unique
 | Let    (Name n) (Exp n p)    [Statement n p]
 -- | Bind source value to name
 | UseSource (Name n)           [Statement n p]

 -- Leaf nodes
 -- | Update a resumable or windowed fold accumulator,
 -- with Exp : acc -> acc
 | Update (Name n) (Exp n p)
 -- | Push to a latest accumulator
 -- with Exp : elem
 | Push   (Name n) (Exp n p)
 deriving (Eq, Ord, Show)



-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program n p) where
 pretty p
  =   vcat (semis $ fmap prettyX (precomps  p)) <> line
  <>  vcat (semis $ fmap pretty  (accums    p)) <> line
  <>                     pretty  (loop      p)  <> line
  <>  vcat (semis $ fmap prettyX (postcomps p)) <> line
  <>  text "return"  <+> pretty  (returns   p)
  where
   semis = fmap (<> text ";")
   prettyX  (a,b) = pretty a <+> text "=" <+> pretty b


instance (Pretty n, Pretty p) => Pretty (Accumulator n p) where
 pretty (Accumulator n acc)
  =   pretty n <+> text "="
  <+> (case acc of
       Resumable t x -> pptx t x <+> text "(Resumable)"
       Windowed  t x -> pptx t x <+> text "(Windowed)"
       Latest    t x -> text "Latest" <+> pretty x <+> text ":" <+> pretty t)
   where
    pptx t x = pretty x <+> text ":" <+> pretty t


instance (Pretty n, Pretty p) => Pretty (Loop n p) where
 pretty (Loop t stmts)
  =  text "for facts :" <+> pretty t <+> text "{" <> line
  <> indent 2 (semis stmts)     <> line
  <> text "}"
  where
   semis = vcat . fmap (<> text ";") . fmap pretty


instance (Pretty n, Pretty p) => Pretty (Statement n p) where
 pretty s
  = case s of
     If x stmts
      -> text "if (" <> pretty x <> text ") {" <> line
      <> semis stmts
      <> text "}"

     IfWindowed i stmts
      -> text "windowed " <> pretty i <> text " {" <> line
      <> semis stmts
      <> text "}"

     Let n x stmts
      -> text "let" <+> pretty n <+> text "=" <+> pretty x <+> text "in {" <> line
      <> semis stmts
      <> text "}"

     UseSource n stmts
      -> text "source as" <+> pretty n <+> text "in {" <> line
      <> semis stmts
      <> text "}"

     Update n x
      -> text "update" <+> pretty n <+> text "with" <+> pretty x

     Push n x
      -> text "push" <+> pretty n <+> text "with" <+> pretty x

  where
   semis stmts = (indent 2 $ vcat $ fmap (<> text ";") $ fmap pretty stmts) <> line
