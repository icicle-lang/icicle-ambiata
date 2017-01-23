{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.IO.Zebra (
    seaOfZebraDriver
  ) where

import qualified Data.List as List

import           Icicle.Data (Attribute (..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.IO.Base
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Psv.Output

import           P


seaOfZebraDriver :: PsvOutputConfig -> [SeaProgramState] -> Either SeaError Doc
seaOfZebraDriver _ states = return $ seaOfDefRead states

-- zebra_read_entity ( zebra_state_t *state, zebra_entity_t *entity ) {
--   /* attribute 0 */
--   zebra_read_entity_0 (0, fleet->iprogram_0, entity)
--
--   /* attribute 1 */
--   zebra_read_entity_1 (1, fleet->iprogram_1, entity)
--   ...
-- }
seaOfDefRead :: [SeaProgramState] -> Doc
seaOfDefRead states = vsep
  [ vsep $ fmap seaOfDefReadProgram states
  , "#line 1 \"read entity\""
  , "static ierror_msg_t zebra_read_entity (zebra_state_t *state, zebra_entity_t *entity)"
  , "{"
  , "    ifleet_t *fleet = state->fleet;"
  , "    iint_t    chord_count = fleet->chord_count;"
  , "    ierror_msg_t error;"
  , ""
  , indent 4 . vsep $ List.zipWith seaOfRead [0..] states
  , "    return 0;"
  , "}"
  , ""
  ]

seaOfRead :: Int -> SeaProgramState -> Doc
seaOfRead ix state = vsep
  [ "/*" <> i <> ": " <> a <> " */"
  , "error = zebra_read_entity_" <> i <> "(state, fleet->mempool, chord_count, " <> i <> ", fleet->iprogram_" <> i <> ", entity);"
  , "if (error) return error;"
  ]
  where
    i = pretty ix
    a = pretty (getAttribute (stateAttribute state))

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
seaOfDefReadProgram :: SeaProgramState -> Doc
seaOfDefReadProgram state = vsep
  [ "#line 1 \"read entity for program" <+> seaOfStateInfo state <> "\""
  , "static ierror_msg_t INLINE"
      <+> pretty (nameOfRead state) <+> "("
      <> "zebra_state_t *state, anemone_mempool_t *mempool, iint_t chord_count, int attribute_ix, "
      <> pretty (nameOfStateType state) <+> "*programs, "
      <> "zebra_entity_t *entity)"
  , "{"
  , "    ierror_msg_t error;"
  , ""
  , "    /* compute each chord */"
  , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
  , "        " <> pretty (nameOfStateType state) <+> "*program = &programs[chord_ix];"
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
  , "        itime_t   *chord_time  = &(program->input." <> pretty (stateTimeVar state) <> ");"
  , "        iint_t    *fact_count  = (iint_t*) chord_time + 1;"
  , "        ierror_t **tombstone   = (ierror_t**) chord_time + 2;"
  , "        void     **input_start = (void**) chord_time + 3;"
  , "        iint_t     input_count = " <> pretty (length (stateInputVars state) - 2) <> "; // minus fact_count and tombstone"
  , "        itime_t  **fact_time   = (itime_t**) input_start + input_count;"
  , ""
  , "        error = zebra_translate "
  , "                  ( state"
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
  , "    }"
  , ""
  , "    return 0; /* no error */"
  , "}"
  , ""
  ]

nameOfRead :: SeaProgramState -> CName
nameOfRead state = pretty ("zebra_read_entity_" <> show (stateName state))
