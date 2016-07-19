{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Debug (
    avalancheOrDie
  , avalancheOrDie'
  ) where

import           Data.Map                      (Map)
import qualified Data.Map                       as Map

import qualified Icicle.Avalanche.Prim.Flat     as A
import qualified Icicle.Avalanche.Program       as A

import           Icicle.Common.Annot           (Annot)
import           Icicle.Common.Base

import           Icicle.Core.Program.Fusion    (FusionError)
import qualified Icicle.Core.Program.Fusion     as C
import qualified Icicle.Core.Program.Program    as C

import           Icicle.Data

import           Icicle.Dictionary

import           Icicle.Internal.Pretty        (pretty)

import qualified Icicle.Pipeline                as P

import qualified Icicle.Source.Parser           as S
import qualified Icicle.Source.Query            as S
import qualified Icicle.Source.Type             as S
import qualified Icicle.Source.Checker          as SC
import qualified Icicle.Source.Transform.Inline as STI

import           Icicle.Storage.Dictionary.Toml

import           P

import           System.IO                     (IO, FilePath)
import           System.IO.Unsafe              (unsafePerformIO)

import           Text.ParserCombinators.Parsec (SourcePos)

import           X.Control.Monad.Trans.Either


data DebugError =
    DebugDictionaryImportError DictionaryImportError
  | DebugCompileError          (P.CompileError P.SourceVar)
  deriving (Show)

------------------------------------------------------------------------

avalancheOrDie :: P.IcicleCompileOptions
               -> FilePath
               -> Text
               -> P.AvalProgramTyped P.SourceVar A.Prim
avalancheOrDie opts dictionaryPath source =
  case Map.minView (avalancheOrDie' opts dictionaryPath [("debug", source)]) of
    Just (x, _) -> x
    Nothing     -> error "avalancheOrDie: program not found"

avalancheOrDie' :: P.IcicleCompileOptions
                -> FilePath
                -> [(Text, Text)]
                -> Map Attribute (P.AvalProgramTyped P.SourceVar A.Prim)
avalancheOrDie' opts dictionaryPath sources = unsafePerformIO $ do
  result <- runEitherT (avalancheFrom opts dictionaryPath sources)
  case result of
    Left (DebugDictionaryImportError x) -> error ("avalancheOrDie: " <> show (pretty x))
    Left (DebugCompileError          x) -> error ("avalancheOrDie: " <> show (pretty x))
    Right xs                            -> pure xs

avalancheFrom :: P.IcicleCompileOptions
              -> FilePath
              -> [(Text, Text)]
              -> EitherT DebugError IO (Map Attribute (P.AvalProgramTyped P.SourceVar A.Prim))
avalancheFrom opts dictionaryPath sources = do
  let checkOpts = P.icicleBigData opts

  dictionary <- firstEitherT DebugDictionaryImportError
              $ loadDictionary checkOpts ImplicitPrelude dictionaryPath
  queries    <- hoistEither
              $ traverse (uncurry (queryOfSource checkOpts dictionary)) sources

  let dictionary' = dictionary
                  { dictionaryEntries =  filter concrete (dictionaryEntries dictionary)
                                      <> fmap (uncurry entryOfQuery) queries }

  avalanche  <- hoistEither (avalancheOfDictionary dictionary')
  return avalanche
  where
    avalancheOfDictionary dict
      = first DebugCompileError
      $ P.avalancheOfDictionary opts dict

    queryOfSource checkOpts dict name src
      = first DebugCompileError
      $ P.queryOfSource checkOpts dict name src "namespace-debug"

    entryOfQuery attr query
      = P.entryOfQuery attr query "namespace-debug"

------------------------------------------------------------------------

concrete :: DictionaryEntry -> Bool
concrete (DictionaryEntry _ (ConcreteDefinition _ _) _) = True
concrete (DictionaryEntry _ (VirtualDefinition  _)   _) = False
