import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial
import qualified Icicle.Test.Core.Exp.Alpha
import qualified Icicle.Test.Core.Exp.Check
import qualified Icicle.Test.Core.Exp.Eval
import qualified Icicle.Test.Core.Program.Eval
import qualified Icicle.Test.Core.Program.Fusion
import qualified Icicle.Test.Core.Program.Condense

import qualified Icicle.Test.Avalanche.EvalCommutes
import qualified Icicle.Test.Avalanche.CheckCommutes

import           Orphanarium.Core.Main


main :: IO ()
main =
  orphanariumMain 
    [ Icicle.Test.Encoding.tests
    , Icicle.Test.Serial.tests
    , Icicle.Test.Core.Exp.Alpha.tests
    , Icicle.Test.Core.Exp.Check.tests
    , Icicle.Test.Core.Exp.Eval.tests
    , Icicle.Test.Core.Program.Eval.tests
    , Icicle.Test.Core.Program.Fusion.tests
    , Icicle.Test.Core.Program.Condense.tests

    , Icicle.Test.Avalanche.EvalCommutes.tests
    , Icicle.Test.Avalanche.CheckCommutes.tests
    ]
