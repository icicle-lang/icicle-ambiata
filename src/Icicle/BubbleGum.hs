-- | Every fact has a different flavour of bubblegum attached to it.
-- It's stuck there and you can't clean it off and it all sort of mixes together.
-- At the end, you can taste the bubblegum to see what flavours went into it
--
-- The actual implementation will have to be smarter, but this is just an abstraction.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.BubbleGum (
      Flavour (..)
    , BubbleGumFact (..)
    , BubbleGumOutput (..)
    , bubbleGumOutputOfFacts
    , bubbleGumNubOutputs
    , mapValue
    ) where

import           Icicle.Common.Base
import           Icicle.Data
import           P

import           Data.List (nub, sort)


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
 = BubbleGumFact        Flavour
 deriving (Show, Eq, Ord)


-- | The bubblegum we get back after evaluating a program.
-- We might get several of these from a single program.
--
-- TODO: add Attribute and reduction name here
data BubbleGumOutput n v
 -- | Result of a *full* reduction, as opposed to a windowed reduction.
 -- There might be multiple reductions in a single feature,
 -- so we want the name of the reduction variable too.
 = BubbleGumReduction   (Name n) v

 -- | List of facts used for windowed reduction or latest
 | BubbleGumFacts       [Flavour]
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


bubbleGumOutputOfFacts :: [BubbleGumFact] -> BubbleGumOutput n v
bubbleGumOutputOfFacts fs
 = BubbleGumFacts
 $ fmap flav fs
 where
  flav (BubbleGumFact f) = f


bubbleGumNubOutputs :: (Ord n, Ord v) => [BubbleGumOutput n v] -> [BubbleGumOutput n v]
bubbleGumNubOutputs os
 = let rs = concatMap reds  os
       fs = concatMap facts os
   in  BubbleGumFacts (nub $ sort fs) : sort rs
 where
  reds b@(BubbleGumReduction{})
   = [b]
  reds _
   = []

  facts (BubbleGumFacts fs)
   = fs
  facts _
   = []


mapValue :: Applicative m => (v -> m v') -> BubbleGumOutput n v -> m (BubbleGumOutput n v')
mapValue f b
 = case b of
    BubbleGumReduction n v -> BubbleGumReduction n <$> f v
    BubbleGumFacts     fs  -> pure (BubbleGumFacts      fs)

