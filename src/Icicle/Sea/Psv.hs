{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.Psv (
    PsvInputConfig(..)
  , PsvOutputConfig(..)
  , PsvMode(..)
  , PsvOutputFormat(..)
  , PsvInputFormat(..)
  , PsvInputAllowDupTime(..)
  , PsvInputDenseDict(..)
  , seaOfPsvDriver
  , defaultMissingValue
  ) where

import qualified Data.List as List

import           Icicle.Common.Type (ValType(..))

import           Icicle.Data (Time)

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base (seaOfAttributeDesc, seaOfTime)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           Icicle.Sea.Psv.Base
import           Icicle.Sea.Psv.Input
import           Icicle.Sea.Psv.Output

import           P



seaOfPsvDriver
  :: [SeaProgramState]
  -> PsvInputConfig
  -> PsvOutputConfig
  -> Either SeaError Doc
seaOfPsvDriver states inputConfig outputConfig = do
  let struct_sea  = seaOfFleetState      states
      alloc_sea   = seaOfAllocFleet      states
      collect_sea = seaOfCollectFleet    states
      config_sea  = seaOfConfigureFleet (inputPsvMode inputConfig) states
  read_sea  <- seaOfReadAnyFact      inputConfig  states
  write_sea <- seaOfWriteFleetOutput outputConfig states
  pure $ vsep
    [ struct_sea
    , ""
    , alloc_sea
    , ""
    , collect_sea
    , ""
    , config_sea
    , ""
    , read_sea
    , ""
    , write_sea
    ]

------------------------------------------------------------------------

seaOfFleetState :: [SeaProgramState] -> Doc
seaOfFleetState states
 = let constTime = "const " <> seaOfValType TimeT
   in vsep
      [ "#line 1 \"fleet state\""
      , "struct ifleet {"
      , indent 4 (defOfVar' 1 "imempool_t" "mempool")         <> ";"
      , indent 4 (defOfVar  0 IntT         "max_chord_count") <> ";"
      , indent 4 (defOfVar  0 IntT         "chord_count")     <> ";"
      , indent 4 (defOfVar' 1 constTime    "chord_times")     <> ";"
      , indent 4 (vsep (fmap defOfProgramState states))
      , indent 4 (vsep (fmap defOfProgramTime  states))
      , "};"
      ]

defOfProgramState :: SeaProgramState -> Doc
defOfProgramState state
 = defOfVar' 1 (pretty (nameOfStateType state))
               (pretty (nameOfProgram state)) <> ";"
 <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

defOfProgramTime :: SeaProgramState -> Doc
defOfProgramTime state
 = defOfVar 0 TimeT (pretty (nameOfLastTime state)) <> ";"
 <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

------------------------------------------------------------------------

seaOfAllocFleet :: [SeaProgramState] -> Doc
seaOfAllocFleet states
 = vsep
 [ "#line 1 \"allocate fleet state\""
 , "static ifleet_t * psv_alloc_fleet (iint_t max_chord_count)"
 , "{"
 , "    ifleet_t *fleet = calloc (1, sizeof (ifleet_t));"
 , ""
 , "    fleet->max_chord_count = max_chord_count;"
 , ""
 , indent 4 (vsep (fmap seaOfAllocProgram states))
 , "    return fleet;"
 , "}"
 ]

seaOfAllocProgram :: SeaProgramState -> Doc
seaOfAllocProgram state
 = let programs  = "fleet->" <> pretty (nameOfProgram state)
       program   = programs <> "[ix]."
       stype     = pretty (nameOfStateType state)

       calloc n t = "calloc (" <> n <> ", sizeof (" <> t <> "));"

       go (n, t) = program <> pretty (newPrefix <> n)
                <> " = "
                <> calloc "psv_max_row_count" (seaOfValType t)

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , programs <> " = " <> calloc "max_chord_count" stype
           , ""
           , "for (iint_t ix = 0; ix < max_chord_count; ix++) {"
           , indent 4 (vsep (fmap go (stateInputVars state)))
           , "}"
           , ""
           ]

------------------------------------------------------------------------

seaOfCollectFleet :: [SeaProgramState] -> Doc
seaOfCollectFleet states
 = vsep
 [ "#line 1 \"collect fleet state\""
 , "static void psv_collect_fleet (ifleet_t *fleet)"
 , "{"
 , "    imempool_t *into_pool       = imempool_create ();"
 , "    imempool_t *last_pool       = fleet->mempool;"
 , "    iint_t      max_chord_count = fleet->max_chord_count;"
 , "    iint_t      chord_count     = fleet->chord_count;"
 , ""
 , indent 4 (vsep (fmap seaOfCollectProgram states))
 , ""
 , "    fleet->mempool = into_pool;"
 , ""
 , "    for (iint_t ix = 0; ix < max_chord_count; ix++) {"
 , indent 8 (vsep (fmap seaOfAssignMempool states))
 , "    }"
 , ""
 , "    if (last_pool != 0) {"
 , "        imempool_free (last_pool);"
 , "    }"
 , "}"
 ]

seaOfAssignMempool :: SeaProgramState -> Doc
seaOfAssignMempool state
 = let pname = pretty (nameOfProgram state)
   in "fleet->" <> pname <> "[ix].mempool = into_pool;"

seaOfCollectProgram :: SeaProgramState -> Doc
seaOfCollectProgram state
 = let pname = pretty (nameOfProgram state)
       stype = pretty (nameOfStateType state)
       pvar  = "program->"

       new n = pvar <> pretty (newPrefix <> n)
       res n = pvar <> pretty (resPrefix <> n)

       copyInputs nts
        = let docs = concatMap copyInput (stateInputVars state)
          in if List.null docs
             then []
             else [ "iint_t new_count = " <> pvar <> "new_count;"
                  , ""
                  , "for (iint_t ix = 0; ix < new_count; ix++) {"
                  , indent 4 $ vsep $ concatMap copyInput nts
                  , "}"
                  ]

       copyInput (n, t)
        | not (needsCopy t)
        = []

        | otherwise
        = [ new n <> "[ix] = " <> prefixOfValType t <> "copy (into_pool, " <> new n <> "[ix]);" ]

       copyResumable (n, t)
        | not (needsCopy t)
        = []

        | otherwise
        = [ ""
          , "if (" <> pvar <> pretty (hasPrefix <> n) <> ") {"
          , indent 4 (res n <> " = " <> prefixOfValType t <> "copy (into_pool, " <> res n <> ");")
          , "}"
          ]

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , "for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
           , indent 4 $ stype <+> "*program = &fleet->" <> pname <> "[chord_ix];"
           , ""
           , "    if (last_pool != 0) {"
           , indent 8 $ vsep $ copyInputs (stateInputVars state)
                            <> concatMap copyResumable (stateResumables state)
           , "    }"
           , "}"
           ]

needsCopy :: ValType -> Bool
needsCopy = \case
  StringT   -> True
  ArrayT{}  -> True
  BufT{}    -> True

  UnitT     -> False
  BoolT     -> False
  IntT      -> False
  DoubleT   -> False
  TimeT     -> False
  ErrorT    -> False

  -- these should have been melted
  PairT{}   -> False
  OptionT{} -> False
  StructT{} -> False
  SumT{}    -> False
  MapT{}    -> False

------------------------------------------------------------------------

seaOfConfigureFleet :: PsvMode -> [SeaProgramState] -> Doc
seaOfConfigureFleet mode states
 = vsep
 [ "#line 1 \"configure fleet state\""
 , "static ierror_loc_t psv_configure_fleet (const char *entity, size_t entity_size, const ichord_t **chord, ifleet_t *fleet)"
 , "{"
 , "    iint_t max_chord_count = fleet->max_chord_count;"
 , ""
 , "    iint_t         chord_count;"
 , "    const itime_t *chord_times;"
 , ""
 , case mode of
     PsvSnapshot time -> indent 4 (seaOfChordTimes [time])
     PsvChords        -> indent 4 seaOfChordScan
 , ""
 , "    if (chord_count > max_chord_count) {"
 , "        return ierror_loc_format"
 , "            ( 0, 0"
 , "            , \"exceeded maximum number of chords per entity (chord_count = %lld, max_chord_count = %lld)\""
 , "            , chord_count"
 , "            , max_chord_count );"
 , "    }"
 , ""
 , "    fleet->chord_count = chord_count;"
 , "    fleet->chord_times = chord_times;"
 , ""
 , indent 4 (vsep (fmap defOfState states))
 , ""
 , "    for (iint_t ix = 0; ix < chord_count; ix++) {"
 , "        itime_t chord_time = chord_times[ix];"
 , ""
 , indent 8 (vsep (fmap seaOfAssignTime states))
 , "    }"
 , ""
 , indent 4 (vsep (fmap defOfLastTime states))
 , ""
 , "    return 0;"
 , "}"
 ]

defOfState :: SeaProgramState -> Doc
defOfState state
 = let stype  = pretty (nameOfStateType state)
       var    = "*p" <> pretty (stateName state)
       member = "fleet->" <> pretty (nameOfProgram state)
   in stype <+> var <+> "=" <+> member <> ";"

defOfLastTime :: SeaProgramState -> Doc
defOfLastTime state
 = "fleet->" <> pretty (nameOfLastTime state) <+> "= 0;"

seaOfAssignTime :: SeaProgramState -> Doc
seaOfAssignTime state
 = let ptime = "p" <> pretty (stateName state) <> "[ix]." <> pretty (stateTimeVar state)
   in ptime <+> "=" <+> "chord_time;"

seaOfChordTimes :: [Time] -> Doc
seaOfChordTimes times
 = vsep
 [ "static const itime_t entity_times[] = { " <> hcat (punctuate ", " (fmap seaOfTime times)) <> " };"
 , ""
 , "chord_count = " <> int (length times) <> ";"
 , "chord_times = entity_times;"
 ]

seaOfChordScan :: Doc
seaOfChordScan
 = "*chord = ichord_scan (*chord, entity, entity_size, &chord_count, &chord_times);"

