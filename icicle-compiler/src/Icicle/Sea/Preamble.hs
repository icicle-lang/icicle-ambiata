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

import qualified Data.List as List

import           System.IO (FilePath)

seaPreamble :: Doc
seaPreamble
 = vsep
 $ fmap (uncurry seaOfExternal) files
 where
  files
   = List.sortBy (compare `on` fst)
   $ $(embedDir "data/sea/")

seaOfExternal :: FilePath -> ByteString -> Doc
seaOfExternal path bs
 = vsep
 [ "// " <> text path
 , "#line " <> int lineNo <> " \"" <> text path <> "\""
 , text (T.unpack (T.unlines file))
 , ""
 ]
 where
  (includes, file)
   = List.span (T.isPrefixOf "#include \"")
   . T.lines
   $ T.decodeUtf8 bs

  lineNo
   = List.length includes + 1
