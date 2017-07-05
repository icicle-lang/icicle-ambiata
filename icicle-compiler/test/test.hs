import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial
import qualified Icicle.Test.Language

import qualified Icicle.Test.Common.Data

import qualified Icicle.Test.Core.Exp.Alpha
import qualified Icicle.Test.Core.Exp.Check
import qualified Icicle.Test.Core.Exp.Eval
import qualified Icicle.Test.Core.Exp.Simp

import qualified Icicle.Test.Core.Program.Eval
import qualified Icicle.Test.Core.Program.Fusion
import qualified Icicle.Test.Core.Program.Condense

import qualified Icicle.Test.Avalanche.EvalCommutes
import qualified Icicle.Test.Avalanche.CheckCommutes
import qualified Icicle.Test.Avalanche.SimpCommutes
import qualified Icicle.Test.Avalanche.Flatten
import qualified Icicle.Test.Avalanche.Melt
import qualified Icicle.Test.Avalanche.MeltPrim

import qualified Icicle.Test.Data.Time
import qualified Icicle.Test.Internal.EditDistance

import qualified Icicle.Test.Runtime.Array
import qualified Icicle.Test.Runtime.Data.Striped

import qualified Icicle.Test.Source.PrettyParse
import qualified Icicle.Test.Source.Progress
import qualified Icicle.Test.Source.Convert
import qualified Icicle.Test.Source.History
import qualified Icicle.Test.Source.MaxMapSize

import qualified Icicle.Test.Sea.Name
import qualified Icicle.Test.Sea.Psv
import qualified Icicle.Test.Sea.Psv.Schema
import qualified Icicle.Test.Sea.PsvFission
import qualified Icicle.Test.Sea.Zebra
import qualified Icicle.Test.Sea.Seaworthy
import qualified Icicle.Test.Sea.Text

import qualified Icicle.Test.Foreign.Array

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Disorder.Core.Main

import           System.Environment (lookupEnv)


data TestSuite =
  TestSuite {
      suiteName :: String
    , suiteTests :: [IO Bool]
    }

psv :: TestSuite
psv =
  TestSuite "psv" [
      Icicle.Test.Sea.Psv.tests
    ]

sea :: TestSuite
sea =
  TestSuite "sea" [
      Icicle.Test.Sea.Name.tests
    , Icicle.Test.Sea.Psv.Schema.tests
    , Icicle.Test.Sea.PsvFission.tests
    , Icicle.Test.Sea.Seaworthy.tests
    , Icicle.Test.Sea.Text.tests
    , Icicle.Test.Sea.Zebra.tests
    ]

sundry :: TestSuite
sundry =
  TestSuite "sundry" [
      Icicle.Test.Avalanche.CheckCommutes.tests
    , Icicle.Test.Avalanche.EvalCommutes.tests
    , Icicle.Test.Avalanche.Flatten.tests
    , Icicle.Test.Avalanche.Melt.tests
    , Icicle.Test.Avalanche.MeltPrim.tests
    , Icicle.Test.Avalanche.SimpCommutes.tests
    , Icicle.Test.Common.Data.tests
    , Icicle.Test.Core.Exp.Alpha.tests
    , Icicle.Test.Core.Exp.Check.tests
    , Icicle.Test.Core.Exp.Eval.tests
    , Icicle.Test.Core.Exp.Simp.tests
    , Icicle.Test.Core.Program.Condense.tests
    , Icicle.Test.Core.Program.Eval.tests
    , Icicle.Test.Core.Program.Fusion.tests
    , Icicle.Test.Data.Time.tests
    , Icicle.Test.Encoding.tests
    , Icicle.Test.Foreign.Array.tests
    , Icicle.Test.Internal.EditDistance.tests
    , Icicle.Test.Language.tests
    , Icicle.Test.Runtime.Array.tests
    , Icicle.Test.Runtime.Data.Striped.tests
    , Icicle.Test.Serial.tests
    , Icicle.Test.Source.Convert.tests
    , Icicle.Test.Source.History.tests
    , Icicle.Test.Source.MaxMapSize.tests
    , Icicle.Test.Source.PrettyParse.tests
    , Icicle.Test.Source.Progress.tests
    ]

suites :: Map String TestSuite
suites =
  Map.fromList $ fmap (\x -> (suiteName x, x)) [
      psv
    , sea
    , sundry
    ]

runTestSuite :: TestSuite -> IO ()
runTestSuite x = do
  putStrLn ""
  putStrLn "────────────────────────────────────────────────────────────"
  putStrLn $ "🚀 Running " ++ (suiteName x) ++ " test suite 🚀"
  putStrLn "────────────────────────────────────────────────────────────"
  putStrLn ""
  disorderMain (suiteTests x)

main :: IO ()
main = do
  msuite <- lookupEnv "TEST_SUITE"
  case msuite of
    Nothing ->
      mapM_ runTestSuite $ Map.elems suites
    Just suite ->
      case Map.lookup suite suites of
        Nothing ->
          putStrLn $ "Unknown test suite: " ++ suite
        Just x ->
          runTestSuite x
