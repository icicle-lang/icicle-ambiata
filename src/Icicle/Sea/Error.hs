{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Error (
    SeaError(..)
  ) where

import           Data.Text (Text)

import           Icicle.Common.Base (BaseValue, OutputName)
import           Icicle.Common.Type (ValType, StructType)
import           Icicle.Data (Attribute(..))
import qualified Icicle.Data as D
import           Icicle.Internal.Pretty ((<+>), pretty, text, line)
import           Icicle.Internal.Pretty (Pretty)

import           Jetski

import           P

------------------------------------------------------------------------

data SeaError
  = SeaJetskiError                JetskiError
  | SeaPsvError                   Text
  | SeaProgramNotFound            Attribute
  | SeaFactConversionError        [D.AsAt D.Value] ValType
  | SeaBaseValueConversionError   BaseValue (Maybe ValType)
  | SeaTypeConversionError                   ValType
  | SeaUnsupportedInputType                  ValType
  | SeaInputTypeMismatch                     ValType    [(Text, ValType)]
  | SeaUnsupportedOutputType                 ValType
  | SeaOutputTypeMismatch         OutputName ValType    [ValType]
  | SeaUnsupportedStructFieldType            ValType    [Text]
  | SeaStructFieldsMismatch                  StructType [(Text, ValType)]
  | SeaNoFactLoop
  | SeaNoOutputs
  deriving (Eq, Show)

instance Pretty SeaError where
  pretty = \case
    SeaFactConversionError vs t
     -> text "Cannot convert facts "
     <> pretty (fmap D.fact vs) <+> text ": [" <> pretty t <> text "]"

    SeaBaseValueConversionError v Nothing
     -> text "Cannot convert value " <> pretty v

    SeaBaseValueConversionError v (Just t)
     -> text "Cannot convert value " <> pretty v <+> text ":" <+> pretty t

    SeaTypeConversionError t
     -> text "Cannot convert type " <> pretty t

    SeaUnsupportedInputType t
     -> text "Unsupported input type " <> pretty t

    SeaUnsupportedOutputType t
     -> text "Unsupported output type " <> pretty t

    SeaStructFieldsMismatch st vs
     -> text "Struct type did not match C struct members:" <> line
     <> text "  struct type = " <> pretty st
     <> text "  members     = " <> pretty vs

    SeaUnsupportedStructFieldType t ns
     -> text "Unsupported struct field type" <> line
     <> text "  field type    = " <> pretty t
     <> text "  mapping on to = " <> text (show ns)

    SeaInputTypeMismatch t ns
     -> text "Cannot map input type on its C struct members" <> line
     <> text "  input type    = " <> pretty t
     <> text "  mapping on to = " <> text (show ns)

    SeaOutputTypeMismatch n t ts
     -> text "Cannot map output type on its C struct members" <> line
     <> text "  output name   = " <> pretty n
     <> text "  output type   = " <> pretty t
     <> text "  mapping on to = " <> text (show ts)

    SeaNoFactLoop
     -> text "No fact loop"

    SeaNoOutputs
     -> text "No outputs"

    SeaProgramNotFound attr
     -> text "Program for attribute \"" <> pretty attr <> "\" not found"

    SeaJetskiError (CompilerError _ _ stderr)
     -> pretty stderr

    SeaJetskiError je
     -> pretty (show je)

    SeaPsvError pe
     -> pretty pe
