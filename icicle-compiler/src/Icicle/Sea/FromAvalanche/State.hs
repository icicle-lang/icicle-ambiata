{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Icicle.Sea.FromAvalanche.State (
    SeaProgramState(..)
  , nameOfProgram
  , nameOfProgram'
  , nameOfCount
  , stateOfProgram
  , seaOfState
  , seaOfStateInfo
  , nameOfStateType
  , nameOfStateSize
  , nameOfStateSize'
  , nameOfLastTime
  , stateInputTypeName
  , stateInputName
  , stateInput
  , stateInputNew
  , stateInputTime
  , stateStateTypeName
  , stateNewCount
  , stateInputRes
  , stateInputHas
  , initOfFactStateStruct

  -- * Prefixes for facts/resumables.
  , hasPrefix
  , resPrefix
  , newPrefix
  ) where

import qualified Data.List          as List
import qualified Data.Map           as Map
import qualified Data.Text          as T

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Type

import           Icicle.Data (Attribute (..), FactMode (..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error
import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Exp
import           Icicle.Sea.FromAvalanche.Type

import           P

------------------------------------------------------------------------

data SeaProgramState = SeaProgramState {
    stateName       :: Int
  , stateAttribute  :: Attribute
  , stateTimeVar    :: Text
  , stateInputType  :: ValType
  , stateInputMode  :: FactMode
  , stateInputVars  :: [(Text, ValType)]
  , stateResumables :: [(Text, ValType)]
  , stateOutputs    :: [(OutputName, (ValType, [ValType]))]
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

stateOfProgram
  :: (Pretty n, Eq n)
  => Int
  -> Attribute
  -> Program (Annot a) n Prim
  -> Either SeaError SeaProgramState
stateOfProgram name attrib program
 = case factVarsOfProgram FactLoopNew program of
    Nothing
     -> Left SeaNoFactLoop

    Just (factType, factMode, factVars)
     -> Right SeaProgramState {
          stateName       = name
        , stateAttribute  = attrib
        , stateTimeVar    = textOfName (bindtime program)
        , stateInputType  = factType
        , stateInputMode  = factMode
        , stateInputVars  = fmap (first textOfName) factVars
        , stateResumables = fmap (first textOfName) (Map.toList (resumablesOfProgram program))
        , stateOutputs    = outputsOfProgram program
        }

------------------------------------------------------------------------

nameOfLastTime :: SeaProgramState -> Text
nameOfLastTime state = "last_time_" <> T.pack (show (stateName state))

nameOfProgram :: SeaProgramState -> Text
nameOfProgram state = nameOfProgram' (stateName state)

nameOfProgram' :: Int -> Text
nameOfProgram' name = "iprogram_" <> T.pack (show name)

nameOfCount :: SeaProgramState -> Text
nameOfCount state = "icount_" <> T.pack (show (stateName state))

nameOfStateType :: SeaProgramState -> Text
nameOfStateType state = nameOfProgram state <> "_t"

nameOfStateSize' :: Int -> Text
nameOfStateSize' name = "size_of_state_" <> nameOfProgram' name

nameOfStateSize :: SeaProgramState -> Text
nameOfStateSize state = nameOfStateSize' (stateName state)


seaOfStateInfo :: SeaProgramState -> Doc
seaOfStateInfo state = "#" <> int (stateName state) <+> "-" <+> seaOfAttributeDesc (stateAttribute state)

seaOfState :: SeaProgramState -> Doc
seaOfState state
 = vsep
 [ "#line 1 \"state and input definition" <+> seaOfStateInfo state <> "\""
 , ""
 , defOfFactStruct      state
 , ""
 , defOfFactStateStruct state
 , ""
 , "typedef struct {"
 , "    /* runtime */"
 , indent 4 (defOfVar' 1 "imempool_t" "mempool;")
 , ""
 , "    /* inputs */"
 , indent 4 (defOfVar_ 0 (pretty (stateInputTypeName state)) "input;")
 , indent 4 (defOfVar_ 0 (pretty (stateStateTypeName state)) "fact_state;")
 , ""
 , "    /* outputs */"
 , indent 4 . vsep
            . concat
            . fmap defsOfOutput
            . stateOutputs
            $ state
 , ""
 , "    /* resumables */"
 , indent 4 . vsep
            . fmap defOfResumable
            . stateResumables
            $ state
 , "}" <+> pretty (nameOfStateType state) <> ";"
 , ""
 , "iint_t " <> pretty (nameOfStateSize state) <+> "()"
 , "{"
 , "    return sizeof (" <> pretty (nameOfStateType state) <> ");"
 , "}"
 ]

------------------------------------------------------------------------

-- | Define a struct where the fields are the melted types.
--
defOfFactStruct :: SeaProgramState -> Doc
defOfFactStruct state
  = vsep
  [ "typedef struct {"
  , indent 4 (defOfVar  0 TimeT (pretty (stateTimeVar state) <> ";"))
  , indent 4 (defOfVar  0 IntT  "new_count;")
  , indent 4 (vsep (fmap defOfFactField (stateInputVars state)))
  , "}" <+> pretty (stateInputTypeName state) <> ";"
  ]

defOfFactField :: (Text, ValType) -> Doc
defOfFactField (name, ty)
  = defOfVar_ 0 (seaOfValType ty) (pretty ("*" <> newPrefix <> name <> ";"))

defOfFactStateStruct :: SeaProgramState -> Doc
defOfFactStateStruct state
  = vsep
  [ "typedef struct {"
  , indent 4 (vsep (fmap defOfFactStateField (stateInputVars state)))
  , "}" <+> pretty (stateStateTypeName state) <> ";"
  ]

defOfFactStateField :: (Text, ValType) -> Doc
defOfFactStateField (name, ty)
  = defOfVar_ 0 (seaOfValType ty) (pretty (newPrefix <> name <> ";"))

initOfFactStateStruct :: SeaProgramState -> Doc
initOfFactStateStruct state
  = vsep (fmap initOfFactStateField (stateInputVars state))

initOfFactStateField :: (Text, ValType) -> Doc
initOfFactStateField (name, ty)
  = "fact_state->" <> pretty (newPrefix <> name) <> " = " <> seaOfXValue (defaultOfType ty) ty <> ";"

------------------------------------------------------------------------

defOfResumable :: (Text, ValType) -> Doc
defOfResumable (n, t)
 =  defOfVar 0 BoolT (pretty hasPrefix <> pretty n) <> semi <> line
 <> defOfVar 0 t     (pretty resPrefix <> pretty n) <> semi

defsOfOutput :: (OutputName, (ValType, [ValType])) -> [Doc]
defsOfOutput (n, (_, ts))
 = List.zipWith (defOfOutputIx n) [0..] ts

defOfOutputIx :: OutputName -> Int -> ValType -> Doc
defOfOutputIx n ix t
 = defOfVar 0 t (seaOfNameIx n ix) <> semi

------------------------------------------------------------------------

-- | Prefix used for the member that represents whether its companion resumable
-- is set or not.
hasPrefix :: Text
hasPrefix = "has_"

-- | Prefix used for the member that represents a resumable.
resPrefix :: Text
resPrefix = "res_"

-- | Prefix for new facts.
newPrefix :: Text
newPrefix = "new_"

stateInputName :: Text
stateInputName
 = "input"

stateInputTypeName :: SeaProgramState -> Text
stateInputTypeName state
 = "input_" <> getAttribute (stateAttribute state) <> "_t"

stateStateTypeName :: SeaProgramState -> Text
stateStateTypeName state
 = "state_" <> getAttribute (stateAttribute state) <> "_t"

stateInput :: Doc
stateInput = pretty stateInputName

stateInputNew :: Doc -> Doc
stateInputNew n = pretty stateInputName <> "." <> pretty newPrefix <> n

stateInputTime :: SeaProgramState -> Doc
stateInputTime state = pretty stateInputName <> "." <> pretty (stateTimeVar state)

stateNewCount :: Doc
stateNewCount = "input.new_count"

-- Resumables are not in the input struct for now.

stateInputRes :: Doc -> Doc
stateInputRes n = pretty resPrefix <> n

stateInputHas :: Doc -> Doc
stateInputHas n = pretty hasPrefix <> n
