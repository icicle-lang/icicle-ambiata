import           Disorder.Core.Main

import           System.Environment (lookupEnv)


main :: IO ()
main = do
  msuite <- lookupEnv "TEST_SUITE"
  case msuite of
    Nothing ->
      disorderCliMain []
    Just "sundry" ->
      disorderCliMain []
    Just _ ->
      pure ()
