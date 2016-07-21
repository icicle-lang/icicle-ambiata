{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Icicle.Compiler.Data where

data IcicleCompileOptions = IcicleCompileOptions
  { icicleInline  :: STI.InlineOption
  , icicleBigData :: SC.CheckOptions
  }

defaultCompileOptions :: IcicleCompileOptions
defaultCompileOptions = IcicleCompileOptions STI.defaultInline SC.defaultCheckOptions
