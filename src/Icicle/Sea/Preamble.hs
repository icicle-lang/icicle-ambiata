{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Preamble (
    seaPreamble
  ) where

import           Icicle.Internal.Pretty as Pretty

import           P

import           Data.ByteString (ByteString)
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
   , seaOfExternal bs
   , ""
   ]

seaOfExternal :: ByteString -> Doc
seaOfExternal
 = text
 . T.unpack
 . T.strip
 . T.unlines
 . filter (not . T.isPrefixOf "#include \"")
 . T.lines
 . T.decodeUtf8
