{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Corpus where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text

import           Icicle.Common.Base
import           Icicle.Common.Type
import qualified Icicle.Common.Type as Type
import           Icicle.Common.Annot

import           Icicle.Test.Arbitrary.Program
import           Icicle.Test.Arbitrary.Source
import           Icicle.Test.Arbitrary.Data

-- import qualified Icicle.Sea.FromAvalanche.Analysis as S
import qualified Icicle.Sea.Eval as S

import           Icicle.Dictionary.Data
import           Icicle.Data
import qualified Icicle.Storage.Dictionary.Toml as DictionaryLoad

import qualified Icicle.Source.Query as Source
import qualified Icicle.Source.Parser as Source

import qualified Icicle.Compiler.Source as Source
import qualified Icicle.Compiler        as Compiler

import           P

import           Data.List (zipWith)

import           Test.QuickCheck

import qualified Icicle.Core                    as C
import qualified Icicle.Source.Lexer.Token as T

import qualified Prelude as Savage

concretes :: [(Text, Encoding)]
concretes =
 [ ("int", IntEncoding)
 , ("string", StringEncoding)
 ]

queries :: [(Text,Text)]
queries =
 [ ("int_oldest"
   , "feature int ~> oldest value")
 , ("string_oldest"
   , "feature string ~> oldest value")
 , ("string_count"
   , "feature string ~> count value")
 , ("string_group"
   , "feature string ~> group value ~> count value")
 , ("string_group2"
   , "feature string ~> group value ~> group value ~> count value")
 ]



wellTypedCorpus :: Gen WellTyped
wellTypedCorpus
 = do (name,core) <- genCore
      wt <- tryGenWellTypedFromCoreEither S.AllowDupTime (streamType core) core
      case wt of
       Left err  -> Savage.error (Text.unpack name <> "\n\n" <> err)
       Right wt' -> return wt'
 where
  streamType c
   = InputType $ C.inputType c -- inputTypeOf (Type.PairT Type.TimeT (C.inputType c))


genCore :: Gen (Text, C.Program () Var)
genCore = do
  (name,src) <- elements queries
  case coreOfSource name src of
   Left u -> Savage.error (Text.unpack name <> ":\n" <> Text.unpack src <> "\n\n" <> u)
   Right core -> return (name, core)

prelude :: Either Savage.String (Source.FunEnvT T.SourcePos Source.Var)
prelude = mconcat <$> nobodyCares (mapM (uncurry $ Source.readIcicleLibrary "check") DictionaryLoad.prelude)

concreteDictionary :: Either Savage.String Dictionary
concreteDictionary
 = do let def enc = ConcreteDefinition enc (Set.singleton tombstone) (ConcreteKey Nothing)
      let entries = fmap (\(k,v) -> DictionaryEntry (Attribute k) (def v) (Namespace namespace)) concretes
      Dictionary entries <$> prelude

coreOfSource :: Text -> Text -> Either Savage.String (C.Program () Var)
coreOfSource name src
 = do d0 <- concreteDictionary
      (_,parsed) <- nobodyCares $ Source.queryOfSource Source.defaultCheckOptions d0 name src namespace
      core0 <- nobodyCares $ Compiler.coreOfSource1 Source.defaultCompileOptions d0 parsed
      let core1 = C.renameProgram varOfVariable core0
      return core1

varOfVariable :: Name T.Variable -> Name Var
varOfVariable = nameOf . fmap (\(T.Variable v) -> Var v 0) . nameBase

tombstone :: Text
tombstone = "💀"

namespace :: Text
namespace = "ns"
