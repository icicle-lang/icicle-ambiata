-- | Working with values and their encodings.
-- Parsing, rendering etc.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Encoding (
    DecodeError (..)
  , renderDecodeError
  , renderValue
  , parseValue
  , valueSatisfiesEncoding
  , primitiveEncoding
  , valueOfJSON
  , jsonOfValue
  , parseFact
  ) where

import           Data.Attoparsec.ByteString
import           Data.Text              as T
import           Data.Text.Read         as T
import           Data.Text.Encoding     as T

import qualified Data.Aeson             as A
import qualified Data.Scientific        as S

import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.ByteString.Lazy   as BS

import           Icicle.Data
import           Icicle.Dictionary

import           P


data DecodeError =
   DecodeErrorBadInput           Text Encoding
 | DecodeErrorMissingStructField Attribute
 | DecodeErrorNotInDictionary    Attribute
 | DecodeErrorValueForVirtual    Attribute
   deriving (Eq, Show)


renderDecodeError :: DecodeError -> Text
renderDecodeError (DecodeErrorBadInput val enc) =
  "Could not decode value '" <> val <> "' of type " <> T.pack (show enc)
renderDecodeError (DecodeErrorMissingStructField attr) =
  "Missing struct field " <> getAttribute attr
renderDecodeError (DecodeErrorNotInDictionary attr) =
  "Given attribute is not in dictionary: " <> getAttribute attr
renderDecodeError (DecodeErrorValueForVirtual attr) =
  "Cannot set values for virtual features: " <> getAttribute attr

primitiveEncoding :: Encoding -> Bool
primitiveEncoding e
 = case e of
   StringEncoding   -> True
   IntEncoding      -> True
   DoubleEncoding   -> True
   BooleanEncoding  -> True
   DateEncoding     -> True
   StructEncoding _ -> False
   ListEncoding   _ -> False


-- | Check whether a value satisfies given encoding.
-- This is only really useful for testing.
valueSatisfiesEncoding :: Value -> Encoding -> Bool
valueSatisfiesEncoding val enc
 = case val of
    StringValue  _
     -> enc == StringEncoding
    IntValue     _
     -> enc == IntEncoding
    DoubleValue  _
     -> enc == DoubleEncoding
    BooleanValue _
     -> enc == BooleanEncoding
    DateValue    _
     -> enc == DateEncoding

    StructValue  (Struct vals)
     | StructEncoding fields <- enc
     -> P.all (valueHasField fields) vals
     && P.all (fieldHasValue vals)   fields
     | otherwise
     -> False

    ListValue    (List ls)
     | ListEncoding le <- enc
     -> P.all (flip valueSatisfiesEncoding le) ls
     | otherwise
     -> False

    Tombstone
     -> True
 where
  valueHasField fields (attr,v)
   | Just f <- P.find ((==attr).attributeOfStructField) fields
   , StructField _ _ e <- f
   = valueSatisfiesEncoding v e
   | otherwise
   = False

  fieldHasValue vals (StructField Mandatory attr _)
   | Just _ <- P.find ((==attr).fst)                    vals
   = True
   | otherwise
   = False

  fieldHasValue _    (StructField Optional _ _)
   = True

   



-- | Render value in a form readable by "parseValue".
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

   StructValue _
    -> json
   ListValue _
    -> json
   Tombstone
    -> tombstone
 where
  json
   = T.decodeUtf8
   $ BS.toStrict
   $ A.encode
   $ jsonOfValue (A.String tombstone) val
   

-- | Attempt to decode value with given encoding.
-- Some values may fit multiple encodings.
parseValue :: Encoding -> Text -> Either DecodeError Value
parseValue e t
 = case e of
    StringEncoding
     -> return (StringValue t)

    IntEncoding
     -> tryDecode IntValue      (T.signed T.decimal)
    DoubleEncoding
     -> tryDecode DoubleValue   (T.signed T.double)

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
     | Right v <- parsed
     -> valueOfJSON e v
     | otherwise
     -> Left err

    ListEncoding _
     | Right v <- parsed
     -> valueOfJSON e v
     | otherwise
     -> Left err

 where
  tryDecode f p
   = f <$> maybeToRight err (readAll p t)
  err
   = DecodeErrorBadInput t e

  parsed
   = parseOnly A.json
   $ T.encodeUtf8 t


-- | Attempt to decode value from JSON
valueOfJSON :: Encoding -> A.Value -> Either DecodeError Value
valueOfJSON e v
 = case e of
    StringEncoding
     | A.String t <- v
     -> return $ StringValue t
     | A.Number n <- v
     -> return $ StringValue $ T.pack $ show n
     | otherwise
     -> Left err

    IntEncoding
     | A.Number n <- v
     , Just   i <- S.toBoundedInteger n
     -> return $ IntValue $ i
     | otherwise
     -> Left err

    DoubleEncoding
     | A.Number n <- v
     -> return $ DoubleValue $ S.toRealFloat n
     | otherwise
     -> Left err

    BooleanEncoding
     | A.Bool b <- v
     -> return $ BooleanValue b
     | otherwise
     -> Left err
       
    DateEncoding
     -- TODO parse date
     | A.String t <- v
     -> return $ DateValue $ Date t
     | otherwise
     -> Left err

    StructEncoding fields
     | A.Object obj <- v
     ->  StructValue . Struct . P.concat
     <$> mapM (getStructField obj) fields
     | otherwise
     -> Left err

    ListEncoding l
     | A.Array arr <- v
     ->  ListValue . List
     <$> mapM (valueOfJSON l) (V.toList arr)
     | otherwise
     -> Left err

 where
  err
   = DecodeErrorBadInput (T.pack $ show v) e

  getStructField obj field
   = case field of
      StructField Mandatory attr enc
       | Just val <- getField obj attr
       -> do    v' <- valueOfJSON enc val
                return [(attr, v')]
       | otherwise
       -> Left  (DecodeErrorMissingStructField attr)

      StructField Optional  attr enc
       | Just val <- getField obj attr
       -> do    v' <- valueOfJSON enc val
                return [(attr, v')]
       | otherwise
       -> return []

  getField obj attr
   = HM.lookup (getAttribute attr) obj
      

jsonOfValue :: A.Value -> Value -> A.Value
jsonOfValue tombstone val
 = case val of
    StringValue v
     -> A.String v
    IntValue v
     -> A.Number $ P.fromIntegral v
    DoubleValue v
     -> A.Number $ S.fromFloatDigits v
    BooleanValue v
     -> A.Bool   v
    DateValue    (Date v)
     -- TODO dates
     -> A.String v
    StructValue (Struct sfs)
     -> A.Object $ P.foldl insert HM.empty sfs
    ListValue (List l)
     -> A.Array  $ V.fromList $ fmap (jsonOfValue tombstone) l
    Tombstone
     -> tombstone
 where
  insert hm (attr,v)
   = HM.insert (getAttribute attr) (jsonOfValue tombstone v) hm


-- | Perform read, only succeed if all input is used
readAll :: T.Reader a -> T.Text -> Maybe a
readAll r t
 | Right (v, rest) <- r t
 , T.null rest
 = Just v

 | otherwise
 = Nothing



parseFact :: Dictionary -> Fact' -> Either DecodeError Fact
parseFact (Dictionary dict) fact'
 = do   def <- maybeToRight (DecodeErrorNotInDictionary attr)
                            (P.find ((==attr).fst) dict)
        case snd def of
         ConcreteDefinition enc
          -> factOf <$> parseValue enc (value' fact')
         VirtualDefinition _
          -> Left (DecodeErrorValueForVirtual attr)

 where
  attr = attribute' fact'

  factOf v
   = Fact
    { entity    = entity'    fact'
    , attribute = attribute' fact'
    , value     = v
    }

