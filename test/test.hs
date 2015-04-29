import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial
import qualified Icicle.Test.Core.Exp.Check
import qualified Icicle.Test.Core.Exp.Eval
import qualified Icicle.Test.Core.Program.Eval

import           Orphanarium.Core.Main


main :: IO ()
main =
  orphanariumMain 
    [ Icicle.Test.Encoding.tests
    , Icicle.Test.Serial.tests
    , Icicle.Test.Core.Exp.Check.tests
    , Icicle.Test.Core.Exp.Eval.tests
    , Icicle.Test.Core.Program.Eval.tests
    ]
