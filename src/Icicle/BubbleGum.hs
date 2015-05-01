-- | Every fact has a different flavour of bubblegum attached to it.
-- It's stuck there and you can't clean it off and it all sort of mixes together.
-- At the end, you can taste the bubblegum to see what flavours went into it
--
-- The actual implementation will have to be smarter, but this is just an abstraction.
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.BubbleGum (
      Flavour (..)
    , BubbleGumFact (..)
    , BubbleGumOutput (..)
    ) where

import           Icicle.Core.Base
import           Icicle.Data
import           P
import           Data.Text


-- | Each flavour has a unique id, and a date.
-- I don't think the date is strictly necessary,
-- but suspect it will make visual debugging easier.
data Flavour =
  Flavour
  { id      :: Int
  , date    :: DateTime
  }
 deriving (Show, Eq, Ord)


-- | Piece of bubblegum attached to a single fact.
-- These are used as inputs to a program.
data BubbleGumFact
 = BubbleGumFact        Flavour Attribute


-- | The bubblegum we get back after evaluating a program.
-- We might get several of these.
data BubbleGumOutput
 -- | Result of a *full* reduction, as opposed to a windowed reduction. 
 -- There might be multiple reductions in a single feature,
 -- so we want the name of the reduction variable too.
 = BubbleGumReduction   Attribute (Name Text) Value

 -- | List of facts used for windowed reduction or latest
 | BubbleGumWindow      [Flavour] Attribute

