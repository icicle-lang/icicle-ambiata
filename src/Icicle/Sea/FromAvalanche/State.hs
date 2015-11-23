{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.State (
    SeaProgramState(..)
  , nameOfProgram
  , nameOfProgram'
  , stateOfProgram
  , seaOfState
  , seaOfStateInfo
  , nameOfStateType
  , stateWordsOfProgram

  -- * Prefixes for facts/resumables.
  , hasPrefix
  , resPrefix
  , newPrefix
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Type

import           Icicle.Data

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error
import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Type

import           P

------------------------------------------------------------------------

data SeaProgramState = SeaProgramState {
    stateName       :: Int
  , stateAttribute  :: Attribute
  , stateDateVar    :: Text
  , stateInputType  :: ValType
  , stateInputVars  :: [(Text, ValType)]
  , stateResumables :: [(Text, ValType)]
  , stateOutputs    :: [(OutputName, (ValType, [ValType]))]
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

stateOfProgram
  :: (Pretty n, Ord n)
  => Int
  -> Attribute
  -> Program (Annot a) n Prim
  -> Either SeaError SeaProgramState
stateOfProgram name attrib program
 = case factVarsOfProgram FactLoopNew program of
    Nothing
     -> Left SeaNoFactLoop

    Just (factType, factVars)
     -> Right SeaProgramState {
          stateName       = name
        , stateAttribute  = attrib
        , stateDateVar    = textOfName (binddate program)
        , stateInputType  = factType
        , stateInputVars  = fmap (first textOfName) factVars
        , stateResumables = fmap (first textOfName) (Map.toList (resumablesOfProgram program))
        , stateOutputs    = outputsOfProgram program
        }

------------------------------------------------------------------------

nameOfProgram :: SeaProgramState -> Text
nameOfProgram state = nameOfProgram' (stateName state)

nameOfProgram' :: Int -> Text
nameOfProgram' name = "iprogram_" <> T.pack (show name)

nameOfStateType :: SeaProgramState -> Text
nameOfStateType state = nameOfProgram state <> "_t"

seaOfStateInfo :: SeaProgramState -> Doc
seaOfStateInfo state = "#" <> int (stateName state) <+> "-" <+> seaOfAttributeDesc (stateAttribute state)

seaOfState :: SeaProgramState -> Doc
seaOfState state
 = vsep
 [ "#line 1 \"state definition" <+> seaOfStateInfo state <> "\""
 , "typedef struct {"
 , "    /* runtime */"
 , indent 4 (defOfVar' 1 "imempool_t" "mempool;")
 , ""
 , "    /* inputs */"
 , indent 4 (defOfVar 0 DateTimeT (pretty (stateDateVar state) <> ";"))
 , indent 4 (defOfVar 0 IntT      "new_count;")
 , indent 4 . vsep
            . fmap defOfFactVar
            . stateInputVars
            $ state
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
 ]

------------------------------------------------------------------------

stateWordsOfProgram :: Ord n => Program (Annot a) n Prim -> Int
stateWordsOfProgram program
 = 1 -- mempool*
 + 1 -- gen_date
 + 1 -- new_count
 + length (maybe [] snd (factVarsOfProgram FactLoopNew program))
 + sum (fmap (length . snd . snd) (outputsOfProgram program))
 + 2 * Map.size (resumablesOfProgram program)

defOfResumable :: (Text, ValType) -> Doc
defOfResumable (n, t)
 =  defOfVar 0 BoolT (pretty hasPrefix <> pretty n) <> semi <> line
 <> defOfVar 0 t     (pretty resPrefix <> pretty n) <> semi

defOfFactVar :: (Text, ValType) -> Doc
defOfFactVar (n, t)
 = defOfVar 1 t (pretty newPrefix <> pretty n) <> semi

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
