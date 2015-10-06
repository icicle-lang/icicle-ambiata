import           Disorder.Core.Main

import qualified Icicle.Test.IO.IO

main :: IO ()
main =
  disorderMain [
      Icicle.Test.IO.IO.tests
    ]
