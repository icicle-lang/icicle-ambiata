{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Corpus where

import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Maybe

import           Test.QuickCheck

import           P

import           Icicle.Common.Base
import           Icicle.Data hiding (inputName)
import           Icicle.Dictionary.Data hiding (inputId, outputId)
import           Icicle.Internal.Pretty
import           Icicle.Test.Arbitrary.Data
import           Icicle.Test.Arbitrary.Program
import qualified Icicle.Compiler        as Compiler
import qualified Icicle.Compiler.Source as Source
import qualified Icicle.Core as Core
import qualified Icicle.Data as Data
import qualified Icicle.Sea.Eval as Sea
import qualified Icicle.Sea.Data as Sea
import qualified Icicle.Source.Lexer.Token as T
import qualified Icicle.Storage.Dictionary.Toml as DictionaryLoad

import qualified Prelude as Savage


corpusInputs :: [(InputName, Encoding)]
corpusInputs =
 [ ([inputname|int|]
   , IntEncoding)
 , ([inputname|string|]
   , StringEncoding)
 , ([inputname|injury|]
   , StructEncoding [field "location" StringEncoding, optfield "action" StringEncoding, field "severity" IntEncoding])
 ]
 where
  optfield name encoding
   = Data.StructField Data.Optional name encoding
  field name encoding
   = Data.StructField Data.Mandatory name encoding

corpusInputId :: InputName -> InputId
corpusInputId = InputId [namespace|corpus|]

corpusQueries :: [(InputName, OutputId, Text)]
corpusQueries =
  [
    -- Tombstones/no values treament between Core and C

    ( [inputname|int|]
    , [outputid|tombstone:bool|]
    , "feature int ~> fold x = False : True ~> x")

  , ( [inputname|injury|]
    , [outputid|tombstone:group|]
    , "feature injury ~> group location ~> count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:group_group|]
    , "feature injury ~> group location ~> group location ~> count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:group_group_group|]
    , "feature injury ~> group location ~> group location ~> group location ~> count location ")

  , ( [inputname|int|]
    , [outputid|tombstone:filter|]
    , "feature int ~> filter tombstone ~> count value")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group|]
    , "feature injury ~> filter tombstone ~> group location ~> count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group_group|]
    , "feature injury ~> filter tombstone ~> group location ~> group location ~> count location ")

  , ( [inputname|injury|]
    , [outputid|tombstone:filter_group_group_group|]
    , "feature injury ~> filter tombstone ~> group location ~> group location ~> group location ~> count location ")

    -- Basic queries

  , ( [inputname|int|]
    , [outputid|corpse:int_oldest|]
    , "feature int ~> oldest value")

  , ( [inputname|string|]
    , [outputid|corpse:string_oldest|]
    , "feature string ~> oldest value")

  , ( [inputname|string|]
    , [outputid|corpse:string_count|]
    , "feature string ~> count value")

  , ( [inputname|string|]
    , [outputid|corpse:string_group|]
    , "feature string ~> group value ~> count value")

  , ( [inputname|string|]
    , [outputid|corpse:string_group_group|]
    , "feature string ~> group value ~> group value ~> count value")

  -- Sane Groups

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_group|]
    , "feature injury ~> group location ~> group action ~> sum severity")

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_group_group|]
    , "feature injury ~> group location ~> group action ~> group severity ~> sum severity")

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_pair|]
    , "feature injury ~> group (location, action) ~> sum severity")

  -- CEO Groups

  , ( [inputname|injury|]
    , [outputid|corpse:injury_group_crazy|]
    , Text.unlines
    [ "feature injury"
    , "~> windowed 7 days"
    , "~> let is_homer = location == \"homer\""
    , "~> fold x = (map_create, None)"
    , "   : case tombstone"
    , "     | True -> (map_create, Some time)"
    , "     | False ->"
    , "       case snd x"
    , "       | None ->"
    , "         case is_homer"
    , "         | True -> (map_insert location severity (fst x), Some time)"
    , "         | False -> (fst x, Some time)"
    , "         end"
    , "       | Some t ->"
    , "         case time == t"
    , "         | True ->"
    , "           case is_homer"
    , "           | True -> (map_insert location severity (fst x), Some time)"
    , "           | False -> (fst x, Some time)"
    , "           end"
    , "         | False ->"
    , "           case is_homer"
    , "           | True -> (map_insert location severity map_create, Some time)"
    , "           | False -> (map_create, Some time)"
    , "           end"
    , "         end"
    , "       end"
    , "     end"
    , "~> (group fold (k, v) = fst x ~> min v)"
    ])
  ]

genWellTypedClusterFromSource :: (InputName, OutputId, Text) -> Gen WellTypedCluster
genWellTypedClusterFromSource (inputName, outputId, src) =
  case coreOfSource outputId src of
    Left u ->
      Savage.error . show . vsep $
        [ "Corpus program cannot be converted to Core:"
        , pretty u
        , text . Text.unpack . renderOutputId $ outputId
        , text . Text.unpack $ src
        ]
    Right core ->
      genWellTypedClusterFromCore (corpusInputId inputName) outputId core

genWellTypedClusterFromCore :: InputId -> OutputId -> Core.Program () Var -> Gen WellTypedCluster
genWellTypedClusterFromCore inputId outputId core = do
  wta <- tryGenAttributeFromCore' core
  case wta of
   Left err  ->
     Savage.error . show . vsep $
       [ "Generating attribute for corpus failed: "
       , text . Text.unpack . renderOutputId $ outputId
       , pretty err
       ]
   Right w ->
     return $ w { wtCluster = (wtCluster w) { Sea.clusterInputId = inputId} }

testAllCorpus :: Show a => Sea.InputAllowDupTime -> (WellTyped -> Gen a) -> (WellTyped -> a -> Property) -> Property
testAllCorpus dup genExtras prop =
  conjoin . flip fmap corpusQueries $ \query ->
    forAll (genWellTypedClusterFromSource query) $ \cluster ->
    forAll (genWellTypedForSingleAttribute dup cluster) $ \wt ->
    forAll (genExtras wt) $
      prop wt

--------------------------------------------------------------------------------

prelude :: Either Savage.String [DictionaryFunction]
prelude =
  DictionaryLoad.fromFunEnv . mconcat <$>
    nobodyCares (mapM (uncurry $ Source.readIcicleLibrary "check") DictionaryLoad.prelude)

inputDictionary :: Either Savage.String Dictionary
inputDictionary =
  let
    mkEntry (n, v) =
      DictionaryInput (corpusInputId n) v (Set.singleton tombstone) (InputKey Nothing)
  in
    Dictionary
      (mapOfInputs $ fmap mkEntry corpusInputs)
      (mapOfOutputs [])
      <$> prelude

coreOfSource :: OutputId -> Text -> Either Savage.String (Core.Program () Var)
coreOfSource oid src
 = do d0 <- inputDictionary
      parsed <- nobodyCares $ Source.queryOfSource Source.defaultCheckOptions d0 oid src
      core0 <- nobodyCares $ Compiler.coreOfSource1 Source.defaultCompileOptions d0 parsed
      let core1 = Core.renameProgram varOfVariable core0
      return core1

varOfVariable :: Name T.Variable -> Name Var
varOfVariable = nameOf . fmap (\(T.Variable v) -> Var v 0) . nameBase
