import           Disorder.Core.Main

import qualified Icicle.Test.IO.IO

import           System.Environment (lookupEnv)


main :: IO ()
main = do
  msuite <- lookupEnv "TEST_SUITE"
  case msuite of
    Nothing ->
      runTests
    Just "sundry" ->
      runTests
    Just _ ->
      pure ()

runTests :: IO ()
runTests =
  disorderMain [
      Icicle.Test.IO.IO.tests
    ]
