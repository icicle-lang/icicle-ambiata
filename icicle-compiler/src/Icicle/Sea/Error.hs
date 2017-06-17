{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Error (
    SeaError(..)
  ) where

import           Data.Map  (Map)
import qualified Data.Map as Map

import           GHC.IO.Exception (IOError)

import           Icicle.Common.Base (BaseValue, OutputName)
import           Icicle.Common.Type (ValType, StructType)
import           Icicle.Data (Attribute(..))
import qualified Icicle.Data as D
import           Icicle.Internal.Pretty ((<+>), pretty, text, vsep, indent)
import           Icicle.Internal.Pretty (Pretty)

import qualified Piano

import qualified Zebra.Foreign.Util as Zebra
import qualified Zebra.Serial.Binary as Zebra
import qualified Zebra.Factset.Table as Zebra
import qualified Zebra.Factset.Block as Zebra

import           Jetski

import           P

------------------------------------------------------------------------

data SeaError
  = SeaJetskiError                JetskiError
  | SeaUnknownInput
  | SeaPsvError                   Text
  | SeaPianoError                 Piano.ParserError
  | SeaZebraError                 Text
  | SeaZebraForeignError          Zebra.ForeignError
  | SeaZebraDecodeError           Zebra.BinaryStripedDecodeError
  | SeaZebraIOError               IOError
  | SeaZebraBlockTableError       Zebra.BlockTableError
  | SeaZebraEntityError           Zebra.EntityError
  | SeaExternalError              Text
  | SeaProgramNotFound            Attribute
  | SeaNoAttributeIndex           Attribute
  | SeaFactConversionError        [D.AsAt D.Value] ValType
  | SeaBaseValueConversionError   BaseValue (Maybe ValType)
  | SeaTypeConversionError                   ValType
  | SeaUnsupportedInputType                  ValType
  | SeaInputTypeMismatch                     ValType    [(Text, ValType)]
  | SeaUnsupportedOutputType                 ValType
  | SeaOutputTypeMismatch         OutputName ValType    [ValType]
  | SeaStructFieldsMismatch                  StructType [(Text, ValType)]
  | SeaDenseFieldNotDefined      Text              [Text]
  | SeaDenseFieldsMismatch        [(Text, ValType)] [(Text, ValType)]
  | SeaDenseFeedNotDefined        Text (Map Text [(Text, ValType)])
  | SeaDenseFeedNotUsed           Text
  | SeaNoFactLoop
  | SeaNoOutputs
  deriving (Eq, Show)

instance Pretty SeaError where
  pretty = \case
    SeaFactConversionError vs t
     -> text "Cannot convert facts "
     <> pretty (fmap D.atFact vs) <+> text ": [" <> pretty t <> text "]"

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
     -> vsep [ "Struct type did not match C struct members:"
             , "  struct type = " <> pretty st
             , "  members     = " <> pretty vs ]

    SeaDenseFieldNotDefined s ss
     -> vsep [ "Dense field does not exist:"
             , "  dense field   = " <> pretty s
             , "  struct fields = " <> pretty ss ]

    SeaDenseFieldsMismatch st vs
     -> vsep [ "Dense fields did not match C struct members:"
             , "  dense fields = " <> pretty st
             , "  members      = " <> pretty vs ]

    SeaDenseFeedNotDefined attr fs
     -> vsep [ "Dense feed not defined in dictionary:"
             , "  dense feeds = " <> pretty (Map.toList fs)
             , "  looking for = " <> pretty attr
             ]
    SeaDenseFeedNotUsed attr
     -> "Dense feed \"" <+> pretty attr <+> "\" is not used in Icicle expressions, nothing to do."

    SeaInputTypeMismatch t ns
     -> vsep [ "Unsupported mapping, cannot map input type to its C struct members"
             , "  input type    = " <> pretty t
             , "  mapping on to = " <> text (show ns) ]

    SeaOutputTypeMismatch n t ts
     -> vsep [ "Unsupported mapping, cannot map output type to its C struct members"
             , "  output name   = " <> pretty n
             , "  output type   = " <> pretty t
             , "  mapping on to = " <> text (show ts) ]

    SeaNoFactLoop
     -> text "No fact loop"

    SeaNoOutputs
     -> text "No outputs"

    SeaProgramNotFound attr
     -> text "Program for attribute \"" <> pretty attr <> "\" not found"

    SeaNoAttributeIndex attr
     -> text "Attribute \"" <> pretty attr <> "\" not found in dictionary"

    SeaJetskiError (CompilerError _ _ stderr)
     -> pretty stderr

    SeaJetskiError je
     -> pretty (show je)

    SeaUnknownInput
     -> text "Unsupported input format"

    SeaPsvError pe
     -> pretty pe

    SeaPianoError pe
     -> vsep [ "Piano error:"
             , indent 2 . pretty $ Piano.renderParserError pe
             ]

    SeaZebraError pe
     -> vsep [ "Zebra error:"
             , indent 2 $ pretty pe
             ]

    SeaZebraForeignError pe
     -> vsep [ "Zebra foreign error:"
             , indent 2 . text . show $ pe
             ]

    SeaZebraDecodeError pe
     -> vsep [ "Zebra decode error:"
             , indent 2 . pretty $ Zebra.renderBinaryStripedDecodeError pe
             ]

    SeaZebraIOError pe
     -> vsep [ "Zebra IO error:"
             , indent 2 . text . show $ pe
             ]

    SeaZebraBlockTableError pe
     -> vsep [ "Zebra block table error:"
             , indent 2 . pretty $ Zebra.renderBlockTableError pe
             ]

    SeaZebraEntityError pe
     -> vsep [ "Zebra entity error:"
             , indent 2 . text . show $ pe
             ]

    SeaExternalError pe
     -> pretty pe
