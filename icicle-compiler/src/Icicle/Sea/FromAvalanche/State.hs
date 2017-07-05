{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
module Icicle.Sea.FromAvalanche.State (
    nameOfCluster
  , nameOfCluster'
  , nameOfKernel
  , nameOfKernel'
  , nameOfCount
  , clusterOfPrograms
  , seaOfClusterState
  , seaOfClusterInfo
  , nameOfClusterState
  , nameOfClusterStateSize
  , nameOfClusterStateSize'
  , nameOfLastTime
  , clusterInputTypeName
  , clusterInputName
  , clusterInput
  , clusterInputNew
  , clusterInputTime
  , clusterNewCount
  , clusterInputRes
  , clusterInputHas
  , nameOfResumable
  , nameOfResumableHasFlagsStart
  , nameOfResumableHasFlagsEnd

  -- * Prefixes for facts/resumables.
  , hasPrefix
  , resPrefix
  , newPrefix
  ) where

import qualified Data.List as List
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Type

import           Icicle.Data.Name

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error
import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Type
import           Icicle.Sea.Name

import           P


clusterOfPrograms ::
     (Pretty n, Eq n)
  => ClusterId
  -> InputId
  -> NonEmpty (Program (Annot a) n Prim)
  -> Either SeaError (Cluster () ())
clusterOfPrograms cid iid programs@(program :| _) =
  case factVarsOfProgram FactLoopNew program of
    Nothing ->
      Left SeaNoFactLoop

    Just (factType, factVars) ->
      Right Cluster {
          clusterId =
            cid

        , clusterInputId =
            iid

        , clusterTimeVar =
            mangle $ bindtime program

        , clusterInputType =
            factType

        , clusterInputVars =
            fmap (first mangle) factVars

        , clusterKernels =
            NonEmpty.zipWith (kernelOfProgram cid) (0 :| [1..]) programs

        , clusterAnnotation =
            ()
        }

kernelOfProgram ::
     (Pretty n, Eq n)
  => ClusterId
  -> KernelIndex
  -> Program (Annot a) n Prim
  -> Kernel ()
kernelOfProgram cid kix program =
  Kernel {
      kernelId =
        KernelId cid kix

    , kernelResumables =
        fmap (first mangle) $ Map.toList (resumablesOfProgram program)

    , kernelOutputs =
        outputsOfProgram program

    , kernelAnnotation =
        () -- FIXME would it ever be interesting to put 'program' here?
    }

------------------------------------------------------------------------

nameOfLastTime :: Cluster c k -> Text
nameOfLastTime cluster =
  "last_time_" <> renderClusterId (clusterId cluster)

nameOfCluster :: Cluster c k -> Text
nameOfCluster cluster =
  nameOfCluster' (clusterId cluster)

nameOfCluster' :: ClusterId -> Text
nameOfCluster' name =
  "icluster_" <> renderClusterId name

nameOfKernel :: Kernel a -> Text
nameOfKernel kernel =
  nameOfKernel' (kernelId kernel)

nameOfKernel' :: KernelId -> Text
nameOfKernel' (KernelId cluster kernel) =
  "icluster_" <> renderClusterId cluster <> "_kernel_" <> renderKernelIndex kernel

nameOfCount :: Cluster c k -> Text
nameOfCount cluster =
  "icount_" <> renderClusterId (clusterId cluster)

nameOfClusterState :: Cluster c k -> Text
nameOfClusterState cluster =
  nameOfCluster cluster <> "_t"

nameOfClusterStateSize' :: ClusterId -> Text
nameOfClusterStateSize' name =
  "size_of_" <> nameOfCluster' name

nameOfClusterStateSize :: Cluster c k -> Text
nameOfClusterStateSize cluster =
  nameOfClusterStateSize' (clusterId cluster)

seaOfClusterInfo :: Cluster c k -> Doc
seaOfClusterInfo cluster =
  "#" <>
  prettyClusterId (clusterId cluster) <+>
  "-" <+>
  prettyText (renderInputId (clusterInputId cluster))

seaOfClusterState :: Cluster c k -> Doc
seaOfClusterState cluster =
  vsep [
      "#line 1 \"cluster state" <+> seaOfClusterInfo cluster <> "\""
    , ""
    , defOfInputStruct cluster
    , ""
    , "typedef struct {"
    , "    /* runtime */"
    , indent 4 (defOfVar' 1 "anemone_mempool_t" "mempool;")
    , indent 4 (defOfVar' 0 "iint_t" "max_map_size;")
    , ""
    , "    /* inputs */"
    , indent 4 (defOfVar_ 0 (pretty (clusterInputTypeName cluster)) "input;")
    , ""
    , vsep . fmap seaOfKernelState . NonEmpty.toList $ clusterKernels cluster
    , ""
    , "}" <+> pretty (nameOfClusterState cluster) <> ";"
    , ""
    , "iint_t " <> pretty (nameOfClusterStateSize cluster) <+> "()"
    , "{"
    , "    return sizeof (" <> pretty (nameOfClusterState cluster) <> ");"
    , "}"
    ]

seaOfKernelState :: Kernel a -> Doc
seaOfKernelState kernel =
  vsep [
      "  /* kernel " <> prettyKernelId (kernelId kernel) <> " */"
    , "    /* outputs */"
    , indent 4 .
        vsep . concat . fmap defsOfOutput $ kernelOutputs kernel
    , ""
    , "    /* resumables: values */"
    , indent 4 .
        vsep . fmap (defValueOfResumable kernel . first renderSeaName) $ kernelResumables kernel
    , ""
    -- Grouping all the has_* flags together lets us set them all to false with a single memset.
    -- Surprisingly, this can save a large amount of compilation time.
    , "    /* resumables: has flags */"
    , indent 4 $
        defOfVar 0 BoolT (nameOfResumableHasFlagsStart kernel) <> semi

    , indent 4 .
        vsep . fmap (defHasOfResumable kernel . first renderSeaName) $ kernelResumables kernel

    , indent 4 $
        defOfVar 0 BoolT (nameOfResumableHasFlagsEnd kernel) <> semi
    , ""
    ]

------------------------------------------------------------------------

-- | Define a struct where the fields are the melted types.
--
defOfInputStruct :: Cluster c k -> Doc
defOfInputStruct cluster =
  vsep [
      "typedef struct {"

    , indent 4 $
        defOfVar 0 TimeT (prettySeaName (clusterTimeVar cluster)) <> ";"

    , indent 4 $
        defOfVar 0 IntT  "new_count;"

    , indent 4 .
        vsep . fmap (defOfFactField . first renderSeaName) $ clusterInputVars cluster

    , "}" <+> pretty (clusterInputTypeName cluster) <> ";"
    ]

defOfFactField :: (Text, ValType) -> Doc
defOfFactField (name, ty) =
  defOfVar_ 0 (seaOfValType ty) (pretty ("*" <> newPrefix <> name <> ";"))

------------------------------------------------------------------------

defValueOfResumable :: Kernel a -> (Text, ValType) -> Doc
defValueOfResumable compute (n, t)
 =  defOfVar 0 t     (pretty resPrefix <> nameOfResumable compute (pretty n)) <> semi

defHasOfResumable :: Kernel a -> (Text, ValType) -> Doc
defHasOfResumable compute (n, _)
 =  defOfVar 0 BoolT (pretty hasPrefix <> nameOfResumable compute (pretty n)) <> semi

nameOfKernelId :: KernelId -> Doc
nameOfKernelId (KernelId cid kix) =
  prettyClusterId cid <> "_" <> prettyKernelIndex kix

nameOfResumable :: Kernel a -> Doc -> Doc
nameOfResumable kernel n =
  nameOfKernelId (kernelId kernel) <> "_" <> n

nameOfResumableHasFlagsStart :: Kernel a -> Doc
nameOfResumableHasFlagsStart kernel =
  "has_flags_start_" <> nameOfKernelId (kernelId kernel)

nameOfResumableHasFlagsEnd :: Kernel a -> Doc
nameOfResumableHasFlagsEnd kernel =
  "has_flags_end_" <> nameOfKernelId (kernelId kernel)

defsOfOutput :: (OutputId, MeltedType) -> [Doc]
defsOfOutput (n, MeltedType _ ts) =
  List.zipWith (defOfOutputIx n) [0..] ts

defOfOutputIx :: OutputId -> Int -> ValType -> Doc
defOfOutputIx n ix t =
  defOfVar 0 t (prettySeaName $ mangleIx n ix) <> semi

------------------------------------------------------------------------

-- | Prefix used for the member that represents whether its companion resumable
-- is set or not.
hasPrefix :: Text
hasPrefix =
  "has_"

-- | Prefix used for the member that represents a resumable.
resPrefix :: Text
resPrefix =
  "res_"

-- | Prefix for new facts.
newPrefix :: Text
newPrefix =
  "new_"

clusterInputName :: Text
clusterInputName =
  "input"

clusterInputTypeName :: Cluster c k -> Text
clusterInputTypeName cluster =
  "input_" <> renderSeaName (mangle (clusterInputId cluster)) <> "_t"

clusterInput :: Doc
clusterInput =
  pretty clusterInputName

clusterInputNew :: Doc -> Doc
clusterInputNew n =
  clusterInput <> "." <> pretty newPrefix <> n

clusterInputTime :: Cluster c k -> Doc
clusterInputTime cluster =
  clusterInput <> "." <> prettySeaName (clusterTimeVar cluster)

clusterNewCount :: Doc
clusterNewCount =
  "input.new_count"

-- Resumables are not in the input struct for now.

clusterInputRes :: Doc -> Doc
clusterInputRes n =
  pretty resPrefix <> n

clusterInputHas :: Doc -> Doc
clusterInputHas n =
  pretty hasPrefix <> n
