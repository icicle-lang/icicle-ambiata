import qualified Icicle.Test.Serial

import           Orphanarium.Core.Main


main :: IO ()
main =
  orphanariumMain [
      Icicle.Test.Serial.tests
    ]
