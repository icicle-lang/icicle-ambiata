-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
module Icicle.Source.Type (
    module X
  ) where

import Icicle.Source.Type.Base          as X
import Icicle.Source.Type.Compounds     as X
import Icicle.Source.Type.Constraints   as X
import Icicle.Source.Type.Subst         as X
import Icicle.Source.Type.Pretty        as X
