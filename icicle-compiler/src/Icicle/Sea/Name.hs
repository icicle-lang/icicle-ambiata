{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Sea.Name (
    SeaName
  , renderSeaName
  , prettySeaName
  , parseSeaName

  , mangle
  , mangleIx
  , unmangle
  ) where

import qualified Data.Text as Text

import           Icicle.Internal.Pretty

import           P

import           Text.Encoding.Z (zEncodeString, zDecodeString)


-- | A legal C identifier.
--
newtype SeaName =
  SeaName {
      unSeaName :: Text
    } deriving (Eq, Ord, Show)

renderSeaName :: SeaName -> Text
renderSeaName =
  unSeaName

prettySeaName :: SeaName -> Doc
prettySeaName =
  prettyText . renderSeaName

seaNameValidHead :: Char -> Bool
seaNameValidHead c =
     (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c == '_')

seaNameValidTail :: Char -> Bool
seaNameValidTail c =
     seaNameValidHead c
  || (c >= '0' && c <= '9')
  || (c == '_')

parseSeaName :: Text -> Maybe SeaName
parseSeaName t =
  case Text.unpack t of
    x : xs | seaNameValidHead x && all seaNameValidTail xs ->
      Just (SeaName t)
    _ ->
      Nothing

mangle :: Pretty n => n -> SeaName
mangle =
  SeaName . Text.pack . zEncodeString . show . pretty

mangleIx :: Pretty n => n -> Int -> SeaName
mangleIx n ix =
  mangle $
    pretty n <> text "/ix/" <> int ix

unmangle :: SeaName -> Text
unmangle =
  Text.pack . zDecodeString . Text.unpack . renderSeaName
