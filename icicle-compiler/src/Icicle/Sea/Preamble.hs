{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Preamble (
    seaPreamble
  ) where

import           Icicle.Internal.Pretty (Doc, vsep, text, int)

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as List

import           P

import qualified Prelude as Savage

import           System.FilePath (takeExtension)
import           System.IO (FilePath)

import           X.Data.FileEmbed (embedWhen)


seaPreamble :: Doc
seaPreamble
 = vsep
 $ fmap (uncurry seaOfExternal) files
 where
  files
   = List.sortBy (compare `on` fst)
   $ $(embedWhen ((== ".h") . takeExtension) "data/sea/")

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
   = case T.decodeUtf8' bs of
       Left err ->
         Savage.error $
           "Icicle.Sea.Preamble.seaOfExternal: failed to decode: " <> path <> "\n" <>
           show err
       Right txt ->
         List.span (T.isPrefixOf "#include \"") $ T.lines txt

  lineNo
   = List.length includes + 1
