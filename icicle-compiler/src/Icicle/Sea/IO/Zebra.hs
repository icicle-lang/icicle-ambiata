{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.IO.Zebra (
    ZebraConfig (..)
  , defaultZebraConfig
  , seaOfZebraDriver
  ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import           Icicle.Data (Attribute (..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.IO.Base
import           Icicle.Sea.FromAvalanche.State

import           P


data ZebraConfig = ZebraConfig
  { zebraChunkFactCount :: !Int }
  deriving (Eq, Show)

defaultZebraConfig :: ZebraConfig
defaultZebraConfig = ZebraConfig (1024*1024)

seaOfZebraDriver :: [Attribute] -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfZebraDriver concretes states = do
  let lookup x = maybeToRight (SeaNoAttributeIndex x) $ List.elemIndex x concretes
  indices <- sequence $ fmap (lookup . stateAttribute) states
  return $ seaOfDefRead (List.zip indices states)

-- zebra_read_entity ( zebra_state_t *state, zebra_entity_t *entity ) {
--   /* attribute 0 */
--   zebra_read_entity_0 (0, fleet->iprogram_0, entity)
--
--   /* attribute 1 */
--   zebra_read_entity_1 (1, fleet->iprogram_1, entity)
--   ...
-- }
seaOfDefRead :: [(Int, SeaProgramAttribute)] -> Doc
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

seaOfRead :: Int -> SeaProgramAttribute -> Doc
seaOfRead index state = vsep
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
    n = pretty (nameOfAttribute state)
    i = pretty index
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
seaOfDefReadProgram :: SeaProgramAttribute -> Doc
seaOfDefReadProgram state = vsep
  [ "#line 1 \"read entity for program" <+> seaOfStateInfo state <> "\""
  , "static ierror_msg_t INLINE"
      <+> pretty (nameOfRead state) <+> "("
      <> "piano_t *piano, zebra_state_t *state, zebra_entity_t *entity, "
      <> "anemone_mempool_t *mempool, iint_t chord_count, int attribute_ix, "
      <> pretty (nameOfStateType state) <+> "*programs)"
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
  , "        /* run compute on the facts read so far */"
  , "        if (state->entity_fact_offset[attribute_ix] != state->entity_fact_count[attribute_ix]) {"
  , indent 12 $ vsep $ fmap (\i -> pretty (nameOfCompute i) <+> " (program);") computes
  , "        }"
  , "    }"
  , ""
  , "    return 0; /* no error */"
  , "}"
  , ""
  ]
 where
  computes = NonEmpty.toList $ stateComputes state

nameOfRead :: SeaProgramAttribute -> CName
nameOfRead state = pretty ("zebra_read_entity_" <> show (stateAttributeName state))
