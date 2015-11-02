{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Preamble (
    seaPreamble
  ) where

import           Icicle.Internal.Pretty as Pretty

import           P

import qualified Data.ByteString.Char8 as BS

import           Data.FileEmbed

seaPreamble :: Doc
seaPreamble
 = vsep
 $ fmap go files
 where
  files
   = $(embedDir "data/sea/")

  go (f,bs)
   = vsep
   [ " // "   <> text f
   , "#line 1 \"" <> text f <> "\""
   , text $ BS.unpack bs ]
