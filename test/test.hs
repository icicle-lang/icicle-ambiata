import qualified Icicle.Test.Encoding
import qualified Icicle.Test.Serial

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
import qualified Icicle.Test.Avalanche.Scoped

-- Superseded
-- import qualified Icicle.Test.Dictionary.Types

import qualified Icicle.Test.Data.DateTime
import qualified Icicle.Test.Internal.EditDistance

import qualified Icicle.Test.Source.PrettyParse
import qualified Icicle.Test.Source.Progress
import qualified Icicle.Test.Source.Convert

-- import qualified Icicle.Test.Sea.Seaworthy
import qualified Icicle.Test.Sea.Psv

import           Disorder.Core.Main


main :: IO ()
main
 = disorderMain
        [ Icicle.Test.Encoding.tests
        , Icicle.Test.Serial.tests

        , Icicle.Test.Common.Data.tests

        , Icicle.Test.Core.Exp.Alpha.tests
        , Icicle.Test.Core.Exp.Check.tests
        , Icicle.Test.Core.Exp.Eval.tests
        , Icicle.Test.Core.Exp.Simp.tests

        , Icicle.Test.Core.Program.Eval.tests
        , Icicle.Test.Core.Program.Fusion.tests
        , Icicle.Test.Core.Program.Condense.tests


        , Icicle.Test.Avalanche.EvalCommutes.tests
        , Icicle.Test.Avalanche.CheckCommutes.tests
        , Icicle.Test.Avalanche.SimpCommutes.tests
        , Icicle.Test.Avalanche.Flatten.tests
        , Icicle.Test.Avalanche.Melt.tests
        , Icicle.Test.Avalanche.Scoped.tests

        -- todo should eventually pass
        -- , Icicle.Test.Sea.Seaworthy.tests
        , Icicle.Test.Sea.Psv.tests

        -- , Icicle.Test.Dictionary.Types.tests

        , Icicle.Test.Data.DateTime.tests
        , Icicle.Test.Internal.EditDistance.tests

        , Icicle.Test.Source.PrettyParse.tests
        , Icicle.Test.Source.Progress.tests
        , Icicle.Test.Source.Convert.tests
        ]
