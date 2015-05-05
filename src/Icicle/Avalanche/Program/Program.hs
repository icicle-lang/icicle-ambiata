-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program.Program (
    Program (..)
  ) where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp
import qualified    Icicle.Core.Program as Core

-- | An entire Avalanche program
data Program n =
  Program
  { precomps    :: [(Name n, Exp n)]
  -- Mutable accumulators
  , accums      :: [Accumulator n]
  , loop        :: Loop n
  , postcomps   :: [(Name n, Exp n)]
  , returns     :: Exp n
  }

-- | Defining or setting up a mutable variable
data Accumulator n =
  Accumulator
  { accName             :: Name n
  -- Initial value
  , accValue            :: Exp n
  -- We might want to read the result of a reduction from history
  , accReadFromHistory  :: Maybe (Name n)
  }

-- | A streaming loop over the inputs.
-- This will be run for every marked value from last execution, plus the new values.
data Loop n =
  Loop  ValType -- ^ What are we iterating over?
        [Statement n]

-- | Part of a loop
data Statement n
 -- We need this value again tomorrow
 = MarkAsUsed
 -- An IF for filters
 | If (Exp n) [Statement n]
 -- Update an accumulator
 | Update (Name n) (Exp n)
 -- Local binding, so the name better be unique
 | Let    (Name n) (Exp n)


------------------------
-- avalancheOfCore :: Core.Program n -> Program n
-- avalancheOfCore _ = ...
--
--
-- alphaEquiv :: Core.Exp -> Core.Exp -> Bool
-- What properties does alpha equivalence have?
--
-- Renaming is still same
-- forall x. rename (prefix "a") x `alphaEquiv` x
--
-- If two expressions evaluate to different values, they can't be alpha equivalent
-- forall x y. x evals to u && y evals to v && u /= v ==> not x `alphaEquiv` y
-- Actually, this is annoyingly false for lambdas, but we could say something about renaming the heap too
--

