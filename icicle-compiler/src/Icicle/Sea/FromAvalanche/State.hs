{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Icicle.Sea.FromAvalanche.State (
    SeaProgramAttribute(..)
  , SeaProgramCompute(..)
  , nameOfAttribute
  , nameOfAttribute'
  , nameOfCompute
  , nameOfCompute'
  , nameOfCount
  , stateOfPrograms
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
  , stateNewCount
  , stateInputRes
  , stateInputHas
  , nameOfResumable
  , nameOfResumableHasFlagsStart
  , nameOfResumableHasFlagsEnd

  -- * Prefixes for facts/resumables.
  , hasPrefix
  , resPrefix
  , newPrefix
  ) where

import qualified Data.List          as List
import           Data.Map (Map)
import qualified Data.Map           as Map
import qualified Data.Text          as T

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ( NonEmpty(..) )

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Type

import           Icicle.Data.Name

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error
import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Type

import           P

------------------------------------------------------------------------

data SeaProgramAttribute = SeaProgramAttribute {
    stateInputIndex     :: Int
  , stateInputId        :: InputId
  , stateInputType      :: ValType
  , stateTimeVar        :: Text
  , stateInputVars      :: [(SeaName, ValType)]
  , stateComputes       :: NonEmpty SeaProgramCompute
  , stateOutputsAll     :: Map OutputId (ValType, [ValType])
  } deriving (Eq, Ord, Show)

data SeaProgramCompute = SeaProgramCompute {
    stateComputeName    :: (Int, Int)
  , stateResumables     :: [(SeaName, ValType)]
  , stateOutputs        :: [(OutputId, (ValType, [ValType]))]
  } deriving (Eq, Ord, Show)


------------------------------------------------------------------------

stateOfPrograms
  :: (Pretty n, Eq n)
  => Int
  -> InputId
  -> NonEmpty (Program (Annot a) n Prim)
  -> Either SeaError SeaProgramAttribute
stateOfPrograms name attrib programs@(program :| _)
 = case factVarsOfProgram FactLoopNew program of
    Nothing
     -> Left SeaNoFactLoop

    Just (factType, factVars)
     -> Right SeaProgramAttribute {
          stateInputIndex  = name
        , stateInputId      = attrib
        , stateTimeVar        = takeSeaName . mangleToSeaName . bindtime $ program
        , stateInputType      = factType
        , stateInputVars      = fmap (first mangleToSeaName) factVars
        , stateComputes       = NonEmpty.zipWith (stateOfProgramCompute name) (0 :| [1..]) programs
        , stateOutputsAll     = Map.fromList . concatMap outputsOfProgram $ NonEmpty.toList programs
        }

stateOfProgramCompute
  :: (Pretty n, Eq n)
  => Int -> Int -> Program (Annot a) n Prim
  -> SeaProgramCompute
stateOfProgramCompute attributeName computeName program
 = SeaProgramCompute {
   stateComputeName = (attributeName, computeName)
 , stateResumables  = fmap (first mangleToSeaName) (Map.toList (resumablesOfProgram program))
 , stateOutputs     = outputsOfProgram program
 }

------------------------------------------------------------------------

nameOfLastTime :: SeaProgramAttribute -> Text
nameOfLastTime state = "last_time_" <> T.pack (show (stateInputIndex state))

nameOfAttribute :: SeaProgramAttribute -> Text
nameOfAttribute state = nameOfAttribute' (stateInputIndex state)

nameOfAttribute' :: Int -> Text
nameOfAttribute' name = "iattribute_" <> T.pack (show name)

nameOfCompute :: SeaProgramCompute -> Text
nameOfCompute state = nameOfCompute' (stateComputeName state)

nameOfCompute' :: (Int,Int) -> Text
nameOfCompute' (attribute,compute) = "icompute_attribute_" <> T.pack (show attribute) <> "_compute_" <> T.pack (show compute)


nameOfCount :: SeaProgramAttribute -> Text
nameOfCount state = "icount_" <> T.pack (show (stateInputIndex state))

nameOfStateType :: SeaProgramAttribute -> Text
nameOfStateType state = nameOfAttribute state <> "_t"

nameOfStateSize' :: Int -> Text
nameOfStateSize' name = "size_of_state_" <> nameOfAttribute' name

nameOfStateSize :: SeaProgramAttribute -> Text
nameOfStateSize state = nameOfStateSize' (stateInputIndex state)


seaOfStateInfo :: SeaProgramAttribute -> Doc
seaOfStateInfo state =
  "#" <>
  int (stateInputIndex state) <+>
  "-" <+>
  (prettyText . takeSeaString . inputIdAsSeaString . stateInputId $ state)

seaOfState :: SeaProgramAttribute -> Doc
seaOfState state
 = vsep
 [ "#line 1 \"state and input definition" <+> seaOfStateInfo state <> "\""
 , ""
 , defOfFactStruct state
 , ""
 , "typedef struct {"
 , "    /* runtime */"
 , indent 4 (defOfVar' 1 "anemone_mempool_t" "mempool;")
 , indent 4 (defOfVar' 0 "iint_t" "max_map_size;")
 , ""
 , "    /* inputs */"
 , indent 4 (defOfVar_ 0 (pretty (stateInputTypeName state)) "input;")
 , ""
 , vsep . fmap seaOfStateCompute
        . NonEmpty.toList
        . stateComputes
        $ state
 , ""
 , "}" <+> pretty (nameOfStateType state) <> ";"
 , ""
 , "iint_t " <> pretty (nameOfStateSize state) <+> "()"
 , "{"
 , "    return sizeof (" <> pretty (nameOfStateType state) <> ");"
 , "}"
 ]

seaOfStateCompute :: SeaProgramCompute -> Doc
seaOfStateCompute state
 = vsep
 [ "  /* compute for " <> pretty (stateComputeName state) <> " */"
 , "    /* outputs */"
 , indent 4 . vsep
            . concat
            . fmap defsOfOutput
            . stateOutputs
            $ state
 , ""
 , "    /* resumables: values */"
 , indent 4 . vsep
            . fmap (defValueOfResumable state . first takeSeaName)
            . stateResumables
            $ state
 , ""
 -- Grouping all the has_* flags together lets us set them all to false with a single memset.
 -- Surprisingly, this can save a large amount of compilation time.
 , "    /* resumables: has flags */"
 , indent 4 ( defOfVar 0 BoolT (nameOfResumableHasFlagsStart state) <> semi )
 , indent 4 . vsep
            . fmap (defHasOfResumable state . first takeSeaName)
            . stateResumables
            $ state
 , indent 4 ( defOfVar 0 BoolT (nameOfResumableHasFlagsEnd state) <> semi )
 , ""
 ]

------------------------------------------------------------------------

-- | Define a struct where the fields are the melted types.
--
defOfFactStruct :: SeaProgramAttribute -> Doc
defOfFactStruct state
  = vsep
  [ "typedef struct {"
  , indent 4 (defOfVar  0 TimeT (pretty (stateTimeVar state) <> ";"))
  , indent 4 (defOfVar  0 IntT  "new_count;")
  , indent 4 (vsep (fmap (defOfFactField . first takeSeaName) (stateInputVars state)))
  , "}" <+> pretty (stateInputTypeName state) <> ";"
  ]

-- TODO use language-c-quote after fixing their savage pretty printer
--  = let fs = fmap defFactField fields
--        t  = T.unpack typename
--    in  [cedecl|typedef struct { $sdecls:fs } $id:t;|]

defOfFactField :: (Text, ValType) -> Doc
defOfFactField (name, ty)
  = defOfVar_ 0 (seaOfValType ty) (pretty ("*" <> newPrefix <> name <> ";"))
-- TODO use language-c-quote after fixing their savage pretty printer
--  = let t = show     (seaOfValType ty)
--        n = T.unpack ("*" <> newPrefix <> name)
--    in  [csdecl|typename $id:t $id:n;|]

------------------------------------------------------------------------

defValueOfResumable :: SeaProgramCompute -> (Text, ValType) -> Doc
defValueOfResumable compute (n, t)
 =  defOfVar 0 t     (pretty resPrefix <> nameOfResumable compute (pretty n)) <> semi

defHasOfResumable :: SeaProgramCompute -> (Text, ValType) -> Doc
defHasOfResumable compute (n, _)
 =  defOfVar 0 BoolT (pretty hasPrefix <> nameOfResumable compute (pretty n)) <> semi

nameOfResumable :: SeaProgramCompute -> Doc -> Doc
nameOfResumable compute n
 = let (i,j) = stateComputeName compute
   in  pretty i <> "_" <> pretty j <> "_" <> n

nameOfResumableHasFlagsStart :: SeaProgramCompute -> Doc
nameOfResumableHasFlagsStart compute
 = let (i,j) = stateComputeName compute
   in  "has_flags_start_" <> pretty i <> "_" <> pretty j

nameOfResumableHasFlagsEnd :: SeaProgramCompute -> Doc
nameOfResumableHasFlagsEnd compute
 = let (i,j) = stateComputeName compute
   in  "has_flags_end_" <> pretty i <> "_" <> pretty j

defsOfOutput :: (OutputId, (ValType, [ValType])) -> [Doc]
defsOfOutput (n, (_, ts))
 = List.zipWith (defOfOutputIx n) [0..] ts

defOfOutputIx :: OutputId -> Int -> ValType -> Doc
defOfOutputIx n ix t
 = defOfVar 0 t (prettyText . takeSeaName . mangleToSeaNameIx n $ ix) <> semi

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

stateInputTypeName :: SeaProgramAttribute -> Text
stateInputTypeName state
 = "input_" <> takeSeaName (mangleToSeaName (stateInputId state)) <> "_t"

stateInput :: Doc
stateInput = pretty stateInputName

stateInputNew :: Doc -> Doc
stateInputNew n = pretty stateInputName <> "." <> pretty newPrefix <> n

stateInputTime :: SeaProgramAttribute -> Doc
stateInputTime state = pretty stateInputName <> "." <> pretty (stateTimeVar state)

stateNewCount :: Doc
stateNewCount = "input.new_count"

-- Resumables are not in the input struct for now.

stateInputRes :: Doc -> Doc
stateInputRes n = pretty resPrefix <> n

stateInputHas :: Doc -> Doc
stateInputHas n = pretty hasPrefix <> n
