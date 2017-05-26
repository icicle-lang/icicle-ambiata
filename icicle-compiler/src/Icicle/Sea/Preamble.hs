{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Icicle.Sea.Preamble (
    seaPreamble
  , seaOfExternal
  ) where

import           Icicle.Internal.Pretty (Doc, vsep, text)

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as List

import           P

import qualified Prelude as Savage

import           System.FilePath (takeExtension)
import           System.IO (FilePath)

import           X.Data.FileEmbed (embedWhen)

import           Anemone.Embed


seaPreamble :: Doc
seaPreamble
 = vsep
 $ fmap (uncurry seaOfExternal) (includes <> externals <> files)
 where
  includes
   = $(embedWhen (== "00-includes.h") "data/sea/")
  files
   = List.sortBy (compare `on` fst)
   $ $(embedWhen (liftA2 (&&) (/= "00-includes.h") ((== ".h") . takeExtension)) "data/sea/")
  externals
   = [ ("anemone_base.h", anemone_base_h)
     , ("anemone_mempool.h", anemone_mempool_h)
     , ("anemone_mempool.c", anemone_mempool_c)
     , ("anemone_grisu2.h", anemone_grisu2_h)
     , ("anemone_grisu2.c", anemone_grisu2_c)
     ]

seaOfExternal :: FilePath -> ByteString -> Doc
seaOfExternal path bs
 = vsep
 [ "// " <> text path
 , "#line 1" <> " \"" <> text path <> "\""
 , text (T.unpack (T.unlines file))
 , ""
 ]
 where
  file
   = case T.decodeUtf8' bs of
       Left err ->
         Savage.error $
           "Icicle.Sea.Preamble.seaOfExternal: failed to decode: " <> path <> "\n" <>
           show err
       Right txt ->
         fmap (\s -> if prefix s then "" else s) (T.lines txt)

  prefix
   = T.isPrefixOf "#include \""
