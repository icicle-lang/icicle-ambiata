{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Icicle.Sea.Eval.Base (
    SeaError(..)
  , Fingerprint(..)
  , UseJetskiCache(..)

  , codeOfPrograms
  , assemblyOfPrograms
  , irOfPrograms

  , getCompilerOptions

  , fromUseJetskiCache
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)

import           Icicle.Common.Annot (Annot)

import           Icicle.Data.Name

import           Icicle.Internal.Pretty (pretty, vsep)
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty)

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Program (seaOfPrograms)
import           Icicle.Sea.FromAvalanche.State (clusterOfPrograms)
import           Icicle.Sea.FromAvalanche.Type (seaOfDefinitions)
import           Icicle.Sea.Header
import           Icicle.Sea.Preamble (seaPreamble)

import           Jetski

import           P hiding (count)

import           System.Environment (lookupEnv)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (hoistEither)


fromUseJetskiCache :: UseJetskiCache -> CacheLibrary
fromUseJetskiCache = \case
  SkipJetskiCache ->
    NoCacheLibrary
  UseJetskiCache ->
    CacheLibrary

getCompilerOptions :: MonadIO m => m [CompilerOption]
getCompilerOptions = do
  menv <- liftIO $ lookupEnv "ICICLE_CC_OPTIONS"
  case menv of
    Nothing  -> return $ defaultCompilerOptions
    Just env -> return $ defaultCompilerOptions <> T.splitOn " " (T.pack env)

defaultCompilerOptions :: [CompilerOption]
defaultCompilerOptions = [
  -- 😊
    "-O1"

  -- all optimisations valid for the current CPU (AVX512, etc)
  , "-march=native"

  -- variable declarations anywhere!
  , "-std=c99"

  -- can't segfault if we exit(1) first!
  , "-DICICLE_ASSERT=1"
  ]

assemblyOfPrograms ::
     (Show a, Show n, Pretty n, Eq n)
  => Fingerprint
  -> [(InputId, NonEmpty (Program (Annot a) n Prim))]
  -> EitherT SeaError IO Text
assemblyOfPrograms fingerprint programs = do
  code <- hoistEither (codeOfPrograms fingerprint programs)
  options <- getCompilerOptions
  firstT SeaJetskiError (compileAssembly options code)

irOfPrograms ::
     (Show a, Show n, Pretty n, Eq n)
  => Fingerprint
  -> [(InputId, NonEmpty (Program (Annot a) n Prim))]
  -> EitherT SeaError IO Text
irOfPrograms fingerprint programs = do
  code <- hoistEither (codeOfPrograms fingerprint programs)
  options <- getCompilerOptions
  firstT SeaJetskiError (compileIR options code)

codeOfPrograms ::
     (Show a, Show n, Pretty n, Eq n)
  => Fingerprint
  -> [(InputId, NonEmpty (Program (Annot a) n Prim))]
  -> Either SeaError Text
codeOfPrograms fingerprint programs = do
  let defs = seaOfDefinitions (concatMap (NonEmpty.toList . snd) programs)

  progs <- zipWithM (\ix (a, p) -> seaOfPrograms ix a p) [0..] programs
  clusters <- zipWithM (\ix (a, p) -> clusterOfPrograms ix a p) [0..] programs

  let
    header0 =
      Header fingerprint clusters

    header =
      renderHeader header0

  pure . (header <>) . textOfDoc . vsep $ [
       "#define ICICLE_NO_INPUT 1"
    , seaPreamble
    , defs
    ] <> progs

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")
