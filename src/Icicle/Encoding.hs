{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Encoding (
    DecodeError (..)
  , renderDecodeError
  , renderValue
  , decodeValue
  ) where

import           Data.Text      as T
import           Data.Text.Read as T

import           Icicle.Data

import           P


data DecodeError =
   DecodeErrorBadInput Text Encoding
 | DecodeErrorUnhandledEncoding Encoding
   deriving (Eq, Show)


renderDecodeError :: DecodeError -> Text
renderDecodeError (DecodeErrorBadInput val enc) =
  "Could not decode value '" <> val <> "' of type " <> T.pack (show enc)
renderDecodeError (DecodeErrorUnhandledEncoding enc) =
  "Could not decode encoding " <> T.pack (show enc)


-- | Render value in a form readable by "decodeValue".
renderValue :: Text -> Value -> Text
renderValue tombstone val
 = case val of
   StringValue v
    -> v
   IntValue v
    -> T.pack $ show v
   DoubleValue v
    -> T.pack $ show v
   BooleanValue v
    -> T.pack $ show v
   DateValue (Date v)
    -> v
   StructValue (Struct vals)
    -> "(" <> T.intercalate "," (fmap renderStructVal vals) <> ")"
   ListValue (List vals)
    -> "[" <> T.intercalate "," (fmap go vals) <> "]"
   Tombstone
    -> tombstone
 where
  go = renderValue tombstone

  renderStructVal (a,v)
   = getAttribute a <> ":" <> go v
   

-- | Attempt to decode value with given encoding.
-- Some values may fit multiple encodings.
decodeValue :: Encoding -> Text -> Either DecodeError Value
decodeValue e t
 = case e of
    StringEncoding
     -- TODO unescape pipes?
     -> return (StringValue t)

    IntEncoding
     -> tryDecode IntValue      T.decimal
    DoubleEncoding
     -> tryDecode DoubleValue   T.double

    BooleanEncoding
     | T.toLower t == "true"
     -> return $ BooleanValue True
     | T.toLower t == "false"
     -> return $ BooleanValue False
     | otherwise
     -> Left err
       
    DateEncoding
     -- TODO parse date
     -> return $ DateValue $ Date t

    StructEncoding _
    -- TODO how should this work?
    -- It seems like we'd need strings to be quoted in order to use this.
    -- Does the "EAVT" format accept structs and lists?
     -> Left (DecodeErrorUnhandledEncoding e)
    ListEncoding   _
     -> Left (DecodeErrorUnhandledEncoding e)
 where
  tryDecode f p
   = f <$> maybeToRight err (readAll p t)
  err
   = DecodeErrorBadInput t e


-- | Perform read, only succeed if all input is used
readAll :: T.Reader a -> T.Text -> Maybe a
readAll r t
 | Right (v, rest) <- r t
 , T.null rest
 = Just v

 | otherwise
 = Nothing

