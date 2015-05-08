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

-- import              Icicle.Internal.Pretty

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


-- instance (Pretty n, Pretty p) => Pretty (Program n p)
