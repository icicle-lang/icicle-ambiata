{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Sea.IO.Offset where

import P

import Icicle.Sea.FromAvalanche.State


-- typedef struct {
--     const char *input_path;
--     iint_t output_fd;
--     iint_t chord_fd;
--     iint_t drop_fd;
--     const char  *error;
--     iint_t fact_count;
--     iint_t entity_count;
--     const size_t output_buffer_size;
--     const size_t chunk_size;
--     const size_t alloc_limit;
-- } zebra_config_t;

zebraConfigCount :: Int
zebraConfigCount = 11

zebraConfigInputPath :: Int
zebraConfigInputPath = 0

zebraConfigOutputFd :: Int
zebraConfigOutputFd = 1

zebraConfigChordFd :: Int
zebraConfigChordFd = 2

zebraConfigDropFd :: Int
zebraConfigDropFd = 3

zebraConfigError :: Int
zebraConfigError = 4

zebraConfigFactCount :: Int
zebraConfigFactCount = 5

zebraConfigEntityCount :: Int
zebraConfigEntityCount = 6

zebraConfigOutputBufferSize :: Int
zebraConfigOutputBufferSize = 7

zebraConfigChunkFactCount :: Int
zebraConfigChunkFactCount = 8

zebraConfigAllocLimitBytes :: Int
zebraConfigAllocLimitBytes = 9

zebraConfigMaxMapSize :: Int
zebraConfigMaxMapSize = 10

--------------------------------------------------------------------------------

-- typedef struct zebra_state {
--     char *output_start;
--     char *output_end;
--     char *output_ptr;
--     iint_t fact_count;
--     iint_t entity_count;
--     ifleet_t *fleet;
--     int output_fd;
--     int drop_fd;
--     int64_t  attribute_count;
--     int64_t *entity_fact_offset;
--     int64_t *entity_fact_count;
--     int64_t  chunk_size;
--     int64_t  alloc_limit;
--     int64_t  entity_alloc_count;
-- } zebra_state_t;

zebraStateCount :: Int
zebraStateCount = 14

zebraStateOutputStart :: Int
zebraStateOutputStart = 0

zebraStateOutputEnd :: Int
zebraStateOutputEnd = 1

zebraStateOutputPtr :: Int
zebraStateOutputPtr = 2

zebraStateFactCount :: Int
zebraStateFactCount = 3

zebraStateEntityCount :: Int
zebraStateEntityCount = 4

zebraStateFleet :: Int
zebraStateFleet = 5

zebraStateOutputFd :: Int
zebraStateOutputFd = 6

zebraStateDropFd :: Int
zebraStateDropFd = 7

zebraStateAttributeCount :: Int
zebraStateAttributeCount = 8

zebraStateEntityFactOffset :: Int
zebraStateEntityFactOffset = 9

zebraStateEntityFactCount :: Int
zebraStateEntityFactCount = 10

zebraStateChunkFactCount :: Int
zebraStateChunkFactCount = 11

zebraStateAllocLimitGB :: Int
zebraStateAllocLimitGB = 12

zebraStateEntityAllocCount :: Int
zebraStateEntityAllocCount = 13

--------------------------------------------------------------------------------

--struct ifleet {
--    anemone_mempool_t *mempool;
--    iint_t           max_chord_count;
--    iint_t           chord_count;
--    itime_t         *chord_times;
--    int64_t         *chord_name_offsets;
--    int64_t         *chord_name_lengths;
--    uint8_t         *chord_name_data;
--    iprogram_0_t    *iprogram_0; /* a_double */
--    iprogram_1_t    *iprogram_1; /* d_string */
--    ...
--    itime_t          last_time_0; /* a_double */
--    itime_t          last_time_1; /* d_string */
--    ...
--    iint_t           icount_0; /* a_double */
--    iint_t           icount_1; /* d_string */
--    ...
--};

fleetMempool :: Int
fleetMempool = 0

fleetMaxChordCount :: Int
fleetMaxChordCount = 1

fleetChordCount :: Int
fleetChordCount = 2

fleetChordTimes :: Int
fleetChordTimes = 3

fleetChordNameOffsets :: Int
fleetChordNameOffsets = 4

fleetChordNameLengths :: Int
fleetChordNameLengths = 5

fleetChordNameData :: Int
fleetChordNameData = 6

fleetProgramOf :: Int -> Int
fleetProgramOf ix = 7 + ix

fleetLastTimeOf :: Int -> Int
fleetLastTimeOf ix = 7 + 2 * ix

fleetCountOf :: Int -> Int
fleetCountOf ix = 7 + 3 * ix

--------------------------------------------------------------------------------

--typedef struct {
--    anemone_mempool_t *mempool;
--    iint_t            max_map_size;
--    input_a_double_t input;
--    ierror_t         a_ix_0;
--    idouble_t        a_ix_1;
--    ibool_t          has_acc_a_conv_4_simpflat_14;
--    idouble_t        res_acc_a_conv_4_simpflat_14;
--    ibool_t          has_acc_a_conv_4_simpflat_13;
--    ierror_t         res_acc_a_conv_4_simpflat_13;
--    ibool_t          has_acc_a_conv_8_simpflat_18;
--    ierror_t         res_acc_a_conv_8_simpflat_18;
--    ibool_t          has_acc_a_conv_8_simpflat_19;
--    idouble_t        res_acc_a_conv_8_simpflat_19;
--    ibool_t          has_acc_a_s_reify_4_conv_9_simpflat_26;
--    idouble_t        res_acc_a_s_reify_4_conv_9_simpflat_26;
--    ibool_t          has_acc_a_s_reify_4_conv_9_simpflat_25;
--    ierror_t         res_acc_a_s_reify_4_conv_9_simpflat_25;
--    ibool_t          has_acc_a_s_reify_4_conv_9_simpflat_27;
--    idouble_t        res_acc_a_s_reify_4_conv_9_simpflat_27;
--} iattribute_0_t;

programMempool :: Int
programMempool = 0

programMaxMapSize :: Int
programMaxMapSize = 1

programInput :: Int
programInput = 2

programOutputStart :: Int -> Int
programOutputStart inputSize = 4 + inputSize

--------------------------------------------------------------------------------

-- typedef struct {
--     itime_t          a_conv_3;
--     iint_t           new_count;
--     ierror_t         *new_a_conv_0_simpflat_96;
--     idouble_t        *new_a_conv_0_simpflat_97;
--     itime_t          *new_a_conv_0_simpflat_98;
-- } input_a_double_t;

inputFactTime :: Int
inputFactTime = 0

inputNewCount :: Int
inputNewCount = 1

inputError :: Int
inputError = 2

inputStart :: Int
inputStart = 3

inputFieldsCount :: SeaProgramAttribute -> Int
inputFieldsCount state =
  length (stateInputVars state) - 2

programInputFactTime :: Int
programInputFactTime = programInput + inputFactTime

programInputNewCount :: Int
programInputNewCount = programInput + inputNewCount

programInputError :: Int
programInputError = programInput + inputError

programInputStart :: Int
programInputStart = programInput + inputStart
