{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Debug (
    avalancheOrDie
  , avalancheOrDie'
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A

import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base

import           Icicle.Core.Program.Fusion (FusionError)
import qualified Icicle.Core.Program.Fusion as C
import qualified Icicle.Core.Program.Program as C

import           Icicle.Data

import           Icicle.Dictionary

import           Icicle.Internal.Pretty (pretty)

import           Icicle.Pipeline

import qualified Icicle.Source.Parser as S
import qualified Icicle.Source.Query as S
import qualified Icicle.Source.Type as S
import qualified Icicle.Source.Checker as SC

import           Icicle.Storage.Dictionary.Toml

import           P

import           System.IO (IO, FilePath)
import           System.IO.Unsafe (unsafePerformIO)

import           Text.ParserCombinators.Parsec (SourcePos)

import           X.Control.Monad.Trans.Either


data DebugError =
    DebugDictionaryImportError DictionaryImportError
  | DebugSourceError    (CompileError SourcePos S.Variable ())
  | DebugFusionError    (FusionError S.Variable)
  | DebugAvalancheError (CompileError () S.Variable A.Prim)
  deriving (Show)

------------------------------------------------------------------------

avalancheOrDie :: SC.CheckOptions -> FilePath -> Text -> A.Program (Annot ()) S.Variable A.Prim
avalancheOrDie checkOpts dictionaryPath source =
  case Map.minView (avalancheOrDie' checkOpts dictionaryPath [("debug", source)]) of
    Just (x, _) -> x
    Nothing     -> error "avalancheOrDie: program not found"

avalancheOrDie' :: SC.CheckOptions -> FilePath -> [(Text, Text)] -> Map Attribute (A.Program (Annot ()) S.Variable A.Prim)
avalancheOrDie' checkOpts dictionaryPath sources = unsafePerformIO $ do
  result <- runEitherT (avalancheFrom checkOpts dictionaryPath sources)
  case result of
    Left (DebugDictionaryImportError x) -> error ("avalancheOrDie: " <> show (pretty x))
    Left (DebugSourceError           x) -> error ("avalancheOrDie: " <> show (pretty x))
    Left (DebugFusionError           x) -> error ("avalancheOrDie: " <> show x)
    Left (DebugAvalancheError        x) -> error ("avalancheOrDie: " <> show (pretty x))
    Right xs                            -> pure xs

avalancheFrom
  :: SC.CheckOptions
  -> FilePath
  -> [(Text, Text)]
  -> EitherT DebugError IO (Map Attribute (A.Program (Annot ()) S.Variable A.Prim))
avalancheFrom checkOpts dictionaryPath sources = do
  dictionary <- firstEitherT DebugDictionaryImportError (loadDictionary checkOpts ImplicitPrelude dictionaryPath)
  queries    <- hoistEither (traverse (uncurry (queryOfSource checkOpts dictionary)) sources)

  let dictionary' = dictionary { dictionaryEntries = filter concrete (dictionaryEntries dictionary)
                                                  <> fmap (uncurry entryOfQuery) queries }

  avalanche  <- hoistEither (avalancheOfDictionary checkOpts dictionary')
  return avalanche

------------------------------------------------------------------------

avalancheOfDictionary :: SC.CheckOptions -> Dictionary -> Either DebugError (Map Attribute (A.Program (Annot ()) S.Variable A.Prim))
avalancheOfDictionary checkOpts dict = do
  let virtuals = fmap (second unVirtual) (getVirtualFeatures dict)

  core      <- traverse (coreOfQuery checkOpts dict) virtuals
  fused     <- traverse fuseCore (Map.unionsWith (<>) core)
  avalanche <- traverse avalancheOfCore fused

  return avalanche

avalancheOfCore :: C.Program () S.Variable -> Either DebugError (A.Program (Annot ()) S.Variable A.Prim)
avalancheOfCore core = do
  flat    <- first DebugAvalancheError (coreFlatten core)
  checked <- first DebugAvalancheError (checkAvalanche flat)
  return checked

fuseCore :: [(S.Variable, C.Program () S.Variable)] -> Either DebugError (C.Program () S.Variable)
fuseCore =
  first DebugFusionError . C.fuseMultiple ()

coreOfQuery
  :: SC.CheckOptions
  -> Dictionary
  -> (Attribute, S.QueryTop (S.Annot SourcePos S.Variable) S.Variable)
  -> Either DebugError (Map Attribute [(S.Variable, C.Program () S.Variable)])
coreOfQuery checkOpts dict (Attribute attr, virtual) =
  first DebugSourceError $ do
    let inlined = sourceInline dict virtual

    desugared    <- sourceDesugarQT inlined
    (checked, _) <- sourceCheckQT checkOpts dict desugared

    let reified = sourceReifyQT checked

    core <- sourceConvert dict reified
    let simplified = coreSimp core

    let baseattr  = (Attribute . unVar . unName) (S.feature virtual)

    pure (Map.singleton baseattr [(S.Variable attr, simplified)])

queryOfSource
  :: SC.CheckOptions
  -> Dictionary
  -> Text
  -> Text
  -> Either DebugError (Attribute, S.QueryTop (S.Annot SourcePos S.Variable) S.Variable)
queryOfSource checkOpts dict name src =
  first DebugSourceError $ do
    parsed       <- sourceParseQT name (Namespace "namespace-debug") src
    desugared    <- sourceDesugarQT parsed
    (checked, _) <- sourceCheckQT checkOpts dict desugared
    pure (Attribute name, checked)

entryOfQuery
  :: Attribute
  -> S.QueryTop (S.Annot SourcePos S.Variable) S.Variable
  -> DictionaryEntry
entryOfQuery attr query =
  DictionaryEntry attr (VirtualDefinition (Virtual query)) (Namespace "namespace-debug")

------------------------------------------------------------------------

concrete :: DictionaryEntry -> Bool
concrete (DictionaryEntry _ (ConcreteDefinition _ _) _) = True
concrete (DictionaryEntry _ (VirtualDefinition  _)   _) = False
