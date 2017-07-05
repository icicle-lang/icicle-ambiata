{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Sea.IO.Zebra (
    ZebraConfig (..)
  , defaultZebraConfig
  , defaultZebraMaxMapSize
  , seaOfZebraDriver
  ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import           Icicle.Data.Name

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Base
import           Icicle.Sea.Name

import           P

import           Text.Printf (printf)


data ZebraConfig = ZebraConfig {
    zebraMaxMapSize      :: !Int
  } deriving (Eq, Show)

defaultZebraConfig :: ZebraConfig
defaultZebraConfig = ZebraConfig defaultZebraMaxMapSize

defaultZebraMaxMapSize :: Int
defaultZebraMaxMapSize = 1024 * 1024

seaOfZebraDriver :: [InputId] -> [Cluster c k] -> Either SeaError Doc
seaOfZebraDriver inputs states = do
  let lookup x = maybeToRight (SeaNoInputIndex x) $ List.elemIndex x inputs
  indices <- sequence $ fmap (lookup . clusterInputId) states
  return . vsep $
    [ seaOfDefRead (List.zip indices states)
    , seaOfInputCount inputs
    ]

seaOfInputCount :: [InputId] -> Doc
seaOfInputCount all_inputs =
  let
    n =
      length all_inputs

    inputs =
      with (List.zip [0..] all_inputs) $ \(ix :: Int, x) ->
        "// " <> pretty (printf "% 5d" ix :: [Char]) <> " = " <> pretty x
  in
    vsep $ [
        "//"
      , "// Expected Input Ordering"
      , "// ======================="
      , "//"
      ] <> inputs <> [
        "//"
      , "static int64_t zebra_attribute_count()"
      , "{"
      , "    return " <> pretty n <> ";"
      , "}"
      , ""
      ]

-- zebra_read_entity ( zebra_state_t *state, zebra_entity_t *entity ) {
--   /* attribute 0 */
--   zebra_read_entity_0 (0, fleet->iprogram_0, entity)
--
--   /* attribute 1 */
--   zebra_read_entity_1 (1, fleet->iprogram_1, entity)
--   ...
-- }
seaOfDefRead :: [(Int, Cluster c k)] -> Doc
seaOfDefRead states = vsep
  [ vsep $ fmap (seaOfDefReadProgram . snd) states
  , "#line 1 \"read entity\""
  , "static ierror_msg_t zebra_read_entity (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity)"
  , "{"
  , "    ifleet_t    *fleet = state->fleet;"
  , "    iint_t       chord_count = fleet->chord_count;"
  , "    ierror_msg_t error;"
  , ""
  , indent 4 . vsep $ fmap (uncurry seaOfRead) states
  , "    return 0;"
  , "}"
  , ""
  ]

seaOfRead :: Int -> Cluster c k -> Doc
seaOfRead index cluster = vsep
  [ "/*" <> n <> ": " <> a <> " */"
  , "error = zebra_read_entity_" <> n
  , "            ( piano"
  , "            , state"
  , "            , entity"
  , "            , fleet->mempool"
  , "            , chord_count"
  , "            , " <> i
  , "            , fleet->" <> n <> ");"
  , "if (error) return error;"
  ]
  where
    n = pretty (nameOfCluster cluster)
    i = pretty index
    a = prettyText . renderInputId $ clusterInputId cluster

-- chords loop:
--
-- zebra_read_entity_0
--   ( anemone_mempool_t *mempool
--   , int chord_count
--   , int attribute_ix
--   , iprogram_0_t *programs
--   , zebra_entity_t *entity ) {
--   for (chords) {
--     zebra_translate (input->first_input_field, input->chord_time, entity);
--     program (input);
--   }
-- }
--
seaOfDefReadProgram :: Cluster c k -> Doc
seaOfDefReadProgram cluster = vsep
  [ "#line 1 \"read entity for program" <+> seaOfClusterInfo cluster <> "\""
  , "static ierror_msg_t INLINE"
      <+> pretty (nameOfRead cluster) <+> "("
      <> "piano_t *piano, zebra_state_t *state, zebra_entity_t *entity, "
      <> "anemone_mempool_t *mempool, iint_t chord_count, int attribute_ix, "
      <> pretty (nameOfClusterState cluster) <+> "*programs)"
  , "{"
  , "    ierror_msg_t error;"
  , "    iint_t max_fact_count = 0;"
  , ""
  , "    /* compute each chord */"
  , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
  , "        " <> pretty (nameOfClusterState cluster) <+> "*program = &programs[chord_ix];"
  , ""
  , "        /* map zebra entity into an input struct:"
  , "           struct input {"
  , "               itime_t   chord_time;"
  , "               iint_t    fact_count;"
  , "               ierror_t *tombstone;"
  , "               ..."
  , "               iint_t   *input_elem_0;"
  , "               ..."
  , "               itime_t  *fact_time;"
  , "           } */"
  , ""
  , "        itime_t   *chord_time  = &(program->input." <> prettySeaName (clusterTimeVar cluster) <> ");"
  , "        iint_t    *fact_count  = (iint_t*) chord_time + 1;"
  , "        ierror_t **tombstone   = (ierror_t**) chord_time + 2;"
  , "        void     **input_start = (void**) chord_time + 3;"
  , "        iint_t     input_count = " <> pretty (length (clusterInputVars cluster) - 2) <> "; // minus fact_count and tombstone"
  , "        itime_t  **fact_time   = (itime_t**) input_start + input_count;"
  , ""
  , "        error = zebra_translate "
  , "                  ( piano"
  , "                  , state"
  , "                  , entity"
  , "                  , mempool"
  , "                  , attribute_ix"
  , "                  , *chord_time"
  , "                  , fact_count"
  , "                  , tombstone"
  , "                  , fact_time"
  , "                  , input_count"
  , "                  , input_start"
  , "                  , entity );"
  , "        if (error) return error;"
  , ""
  , "        max_fact_count = MAX (max_fact_count, *fact_count);"
  , ""
  , "        /* run compute on the facts read so far */"
  , "        if (*fact_count != 0) {"
  , indent 12 $ vsep $ fmap (\i -> pretty (nameOfKernel i) <+> " (program);") kernels
  , "            *fact_count = 0;"
  , "        }"
  , "    }"
  , ""
  , "    state->fact_count += max_fact_count;"
  , "    return 0; /* no error */"
  , "}"
  , ""
  ]
 where
  kernels = NonEmpty.toList $ clusterKernels cluster

nameOfRead :: Cluster c k -> CName
nameOfRead cluster =
  pretty ("zebra_read_entity_" <> pretty (nameOfCluster cluster))
