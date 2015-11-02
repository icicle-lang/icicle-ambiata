{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Preamble (
    seaPreamble
  ) where

import           Icicle.Internal.Pretty as Pretty

import           P

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Data.FileEmbed

import           Data.List (sortBy)

seaPreamble :: Doc
seaPreamble
 = vsep
 $ fmap go files
 where
  files
   = sortBy (compare `on` fst)
   $ $(embedDir "data/sea/")

  go (f,bs)
   = vsep
   [ "// " <> text f
   , "#line 1 \"" <> text f <> "\""
   , text . T.unpack . T.strip $ T.decodeUtf8 bs
   , ""
   ]
