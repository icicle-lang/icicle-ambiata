{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Icicle.Test.Arbitrary.NanEq (
    (=~=)
  , hedgehogNanEq
  ) where

import           Icicle.Common.NanEq

import           Disorder.Jack.Property (Property, counterexample)
import           Disorder.Jack.Property.Diff (renderDiffs)

import qualified Hedgehog
import qualified Hedgehog.Internal.Source as Hedgehog

import           P

import           Text.Show.Pretty (ppShow)
import qualified Text.Show.Pretty as Pretty

--------------------------------------------------------------------------------
-- QuickCheck / Jack

infix 4 =~=

(=~=) :: (NanEq a, Show a) => a -> a -> Property
(=~=) x0 y0 =
  let
    render =
      case (Pretty.reify x0, Pretty.reify y0) of
        (Just x, Just y) ->
          renderDiffs x y
        _ ->
          ppShow x0 <>
          " =/= " <>
          ppShow y0
  in
    counterexample "=== Not NaN equal ===" $
    counterexample render (nanEq x0 y0)

infix 4 `hedgehogNanEq`

hedgehogNanEq :: (NanEq a, Show a, Hedgehog.MonadTest m, Hedgehog.HasCallStack) => a -> a -> m ()
hedgehogNanEq x0 y0 =
  Hedgehog.withFrozenCallStack $
    let
      render =
        case (Pretty.reify x0, Pretty.reify y0) of
          (Just x, Just y) ->
            renderDiffs x y
          _ ->
            ppShow x0 <>
            " =/= " <>
            ppShow y0

    in case nanEq x0 y0 of
        True -> return ()
        False -> do
          Hedgehog.annotate "=== Not NaN equal ==="
          Hedgehog.annotate render
          Hedgehog.failure
