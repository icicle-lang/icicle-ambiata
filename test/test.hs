import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial
import qualified Icicle.Test.Core.Check
import qualified Icicle.Test.Core.Eval

import           Orphanarium.Core.Main


main :: IO ()
main =
  orphanariumMain 
    [ Icicle.Test.Encoding.tests
    , Icicle.Test.Serial.tests
    , Icicle.Test.Core.Check.tests
    , Icicle.Test.Core.Eval.tests
    ]
