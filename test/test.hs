import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial

import           Orphanarium.Core.Main


main :: IO ()
main =
  orphanariumMain 
    [ Icicle.Test.Encoding.tests
    , Icicle.Test.Serial.tests
    ]
