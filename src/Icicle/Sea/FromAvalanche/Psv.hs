{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.Psv (
    PsvConfig(..)
  , PsvMode(..)
  , seaOfPsvDriver
  ) where

import qualified Data.ByteString as B
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word8)

import           Icicle.Avalanche.Prim.Flat (Prim(..), PrimUpdate(..), PrimUnsafe(..))
import           Icicle.Avalanche.Prim.Flat (meltType)

import           Icicle.Common.Base (OutputName(..))
import           Icicle.Common.Type (ValType(..), StructType(..), StructField(..))
import           Icicle.Common.Type (defaultOfType)

import           Icicle.Data (Attribute(..), Time)

import           Icicle.Internal.Pretty
import qualified Icicle.Internal.Pretty as Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base (seaOfAttributeDesc, seaOfTime)
import           Icicle.Sea.FromAvalanche.Base (seaOfNameIx, seaOfChar)
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Program (seaOfXValue)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P
import           Prelude (String)

import           Text.Printf (printf)


------------------------------------------------------------------------

data PsvMode
  = PsvSnapshot Time
  | PsvChords
  deriving (Eq, Ord, Show)

data PsvConfig = PsvConfig {
    psvMode       :: PsvMode
  , psvTombstones :: Map Attribute (Set Text)
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

seaOfPsvDriver :: [SeaProgramState] -> PsvConfig -> Either SeaError Doc
seaOfPsvDriver states config = do
  let struct_sea  = seaOfFleetState                      states
      alloc_sea   = seaOfAllocFleet                      states
      collect_sea = seaOfCollectFleet                    states
      config_sea  = seaOfConfigureFleet (psvMode config) states
  read_sea  <- seaOfReadAnyFact      config           states
  write_sea <- seaOfWriteFleetOutput (psvMode config) states
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

nameOfLastTime :: SeaProgramState -> Text
nameOfLastTime state = "last_time_" <> T.pack (show (stateName state))

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

------------------------------------------------------------------------

seaOfReadAnyFact :: PsvConfig -> [SeaProgramState] -> Either SeaError Doc
seaOfReadAnyFact config states = do
  let tss = fmap (lookupTombstones config) states
  readStates_sea <- zipWithM seaOfReadFact states tss
  pure $ vsep
    [ vsep readStates_sea
    , ""
    , "#line 1 \"read any fact\""
    , "static ierror_loc_t psv_read_fact"
    , "  ( const char   *attrib_ptr"
    , "  , const size_t  attrib_size"
    , "  , const char   *value_ptr"
    , "  , const size_t  value_size"
    , "  , const char   *time_ptr"
    , "  , const size_t  time_size"
    , "  , ifleet_t     *fleet )"
    , "{"
    , indent 4 (vsep (fmap seaOfReadNamedFact states))
    , "    return 0;"
    , "}"
    ]

seaOfReadNamedFact :: SeaProgramState -> Doc
seaOfReadNamedFact state
 = let attrib = getAttribute (stateAttribute state)
       fun    = pretty (nameOfReadFact state)
       pname  = pretty (nameOfProgram  state)
       tname  = pretty (nameOfLastTime state)
   in vsep
      [ "/* " <> pretty attrib <> " */"
      , "if (" <> seaOfStringEq attrib "attrib_ptr" (Just "attrib_size") <> ") {"
      , "    itime_t time;"
      , "    ierror_loc_t error = fixed_read_itime (time_ptr, time_size, &time);"
      , "    if (error) return error;"
      , ""
      , "    ibool_t        ignore_time = itrue;"
      , "    iint_t         chord_count = fleet->chord_count;"
      , "    const itime_t *chord_times = fleet->chord_times;"
      , ""
      , "    /* ignore this time if it comes after all the chord times */"
      , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
      , "        if (chord_times[chord_ix] >= time) {"
      , "            ignore_time = ifalse;"
      , "            break;"
      , "        }"
      , "    }"
      , ""
      , "    if (ignore_time) return 0;"
      , ""
      , "    itime_t last_time = fleet->" <> tname <> ";"
      , ""
      , "    if (time <= last_time) {"
      , "        char curr_time_ptr[text_itime_max_size];"
      , "        size_t curr_time_size = text_write_itime (time, curr_time_ptr);"
      , ""
      , "        char last_time_ptr[text_itime_max_size];"
      , "        size_t last_time_size = text_write_itime (last_time, last_time_ptr);"
      , ""
      , "        return ierror_loc_format"
      , "           ( time_ptr + time_size"
      , "           , time_ptr"
      , "           , \"%.*s: time is out of order: %.*s must be later than %.*s\""
      , "           , attrib_size"
      , "           , attrib_ptr"
      , "           , curr_time_size"
      , "           , curr_time_ptr"
      , "           , last_time_size"
      , "           , last_time_ptr );"
      , "    }"
      , ""
      , "    fleet->" <> tname <> " = time;"
      , ""
      , "    return " <> fun <> " (value_ptr, value_size, time, fleet->mempool, chord_count, fleet->" <> pname <> ");"
      , "}"
      , ""
      ]

------------------------------------------------------------------------

nameOfReadFact :: SeaProgramState -> Text
nameOfReadFact state = T.pack ("psv_read_fact_" <> show (stateName state))

seaOfReadFact :: SeaProgramState -> Set Text -> Either SeaError Doc
seaOfReadFact state tombstones = do
  input     <- checkInputType state
  readInput <- seaOfReadInput input
  pure $ vsep
    [ "#line 1 \"read fact" <+> seaOfStateInfo state <> "\""
    , "static ierror_loc_t INLINE"
        <+> pretty (nameOfReadFact state) <+> "("
        <> "const char *value_ptr, const size_t value_size, itime_t time, "
        <> "imempool_t *mempool, iint_t chord_count, "
        <> pretty (nameOfStateType state) <+> "*programs)"
    , "{"
    , "    ierror_loc_t error;"
    , ""
    , "    char *p  = (char *) value_ptr;"
    , "    char *pe = (char *) value_ptr + value_size;"
    , ""
    , "    ierror_t " <> pretty (inputSumError input) <> ";"
    , indent 4 . vsep . fmap seaOfDefineInput $ inputVars input
    , ""
    , "    " <> align (seaOfReadTombstone input (Set.toList tombstones)) <> "{"
    , "        " <> pretty (inputSumError input) <> " = ierror_not_an_error;"
    , ""
    , indent 8 readInput
    , "    }"
    , ""
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , "        " <> pretty (nameOfStateType state) <+> "*program = &programs[chord_ix];"
    , ""
    , "        /* don't read values after the chord time */"
    , "        if (time > program->" <> pretty (stateTimeVar state) <> ")"
    , "            continue;"
    , ""
    , "        iint_t new_count = program->new_count;"
    , ""
    , "        program->" <> pretty (inputSumError  input) <> "[new_count] = " <> pretty (inputSumError input) <> ";"
    , indent 8 . vsep . fmap seaOfAssignInput $ inputVars input
    , "        program->" <> pretty (inputTime     input) <> "[new_count] = time;"
    , ""
    , "        new_count++;"
    , ""
    , "        if (new_count == psv_max_row_count) {"
    , "             " <> pretty (nameOfProgram state) <> " (program);"
    , "             new_count = 0;"
    , "        } else if (new_count > psv_max_row_count) {"
    , "             return ierror_loc_format (0, 0, \"" <> pretty (nameOfReadFact state) <> ": new_count > max_count\");"
    , "        }"
    , ""
    , "        program->new_count = new_count;"
    , "    }"
    , ""
    , "    return 0; /* no error */"
    , "}"
    , ""
    ]

seaOfAssignInput :: (Text, ValType) -> Doc
seaOfAssignInput (n, _)
 = "program->" <> pretty n <> "[new_count] = " <> pretty n <> ";"

seaOfDefineInput :: (Text, ValType) -> Doc
seaOfDefineInput (n, t)
 = seaOfValType t <+> pretty n <> initType t

initType :: ValType -> Doc
initType vt = " = " <> seaOfXValue (defaultOfType vt) vt <> ";"

------------------------------------------------------------------------

seaOfReadTombstone :: CheckedInput -> [Text] -> Doc
seaOfReadTombstone input = \case
  []     -> Pretty.empty
  (t:ts) -> "if (" <> seaOfStringEq t "value_ptr" (Just "value_size") <> ") {" <> line
         <> "    " <> pretty (inputSumError input) <> " = ierror_tombstone;" <> line
         <> "} else " <> seaOfReadTombstone input ts

------------------------------------------------------------------------

data CheckedInput = CheckedInput {
    inputSumError :: Text
  , inputTime     :: Text
  , inputType     :: ValType
  , inputVars     :: [(Text, ValType)]
  } deriving (Eq, Ord, Show)

checkInputType :: SeaProgramState -> Either SeaError CheckedInput
checkInputType state
 = case stateInputType state of
     PairT (SumT ErrorT t) TimeT
      | (sumError, ErrorT) : xs0 <- stateInputVars state
      , Just vars                <- init xs0
      , Just (time, TimeT)       <- last xs0
      -> Right CheckedInput {
             inputSumError = newPrefix <> sumError
           , inputTime     = newPrefix <> time
           , inputType     = t
           , inputVars     = fmap (first (newPrefix <>)) vars
           }

     t
      -> Left (SeaUnsupportedInputType t)

seaOfReadInput :: CheckedInput -> Either SeaError Doc
seaOfReadInput input
 = case (inputVars input, inputType input) of
    ([(nx, BoolT)], BoolT)
     -> pure (readValue "text" assignVar nx BoolT)

    ([(nx, DoubleT)], DoubleT)
     -> pure (readValue "text" assignVar nx DoubleT)

    ([(nx, IntT)], IntT)
     -> pure (readValue "text" assignVar nx IntT)

    ([(nx, TimeT)], TimeT)
     -> pure (readValue "text" assignVar nx TimeT)

    ([(nx, StringT)], StringT)
     -> pure (readValuePool "text" assignVar nx StringT)

    (_, t@(ArrayT _))
     -> seaOfReadJsonValue assignVar t (inputVars input)

    (_, t@(StructT _))
     -> seaOfReadJsonValue assignVar t (inputVars input)

    (_, t)
     -> Left (SeaUnsupportedInputType t)

------------------------------------------------------------------------

-- Describes how to assign to a C struct member, this changes for arrays
type Assignment = Doc -> ValType -> Doc -> Doc

assignVar :: Assignment
assignVar n _ x = pretty n <+> "=" <+> x <> ";"

assignArray :: Assignment
assignArray n t x = n <+> "=" <+> seaOfArrayPut n "ix" x t <> ";"

seaOfArrayPut :: Doc -> Doc -> Doc -> ValType -> Doc
seaOfArrayPut arr ix val typ
 = seaOfPrimDocApps (seaOfXPrim (PrimUpdate (PrimUpdateArrayPut typ)))
                    [ arr, ix, val ]

seaOfArrayIndex :: Doc -> Doc -> ValType -> Doc
seaOfArrayIndex arr ix typ
 = seaOfPrimDocApps (seaOfXPrim (PrimUnsafe (PrimUnsafeArrayIndex typ)))
                    [ arr, ix ]

------------------------------------------------------------------------

seaOfReadJsonValue :: Assignment -> ValType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonValue assign vtype vars
 = case (vars, vtype) of
     ([(nb, BoolT), nx], OptionT t) -> do
       val_sea <- seaOfReadJsonValue assign t [nx]
       pure $ vsep
         [ "ibool_t is_null;"
         , "error = json_try_read_null (&p, pe, &is_null);"
         , "if (error) return error;"
         , ""
         , "if (is_null) {"
         , indent 4 (assign (pretty nb) BoolT "ifalse")
         , "} else {"
         , indent 4 (assign (pretty nb) BoolT "itrue")
         , ""
         , indent 4 val_sea
         , "}"
         ]

     ([(nx, BoolT)], BoolT)
      -> pure (readValue "json" assign nx BoolT)

     ([(nx, IntT)], IntT)
      -> pure (readValue "json" assign nx IntT)

     ([(nx, DoubleT)], DoubleT)
      -> pure (readValue "json" assign nx DoubleT)

     ([(nx, TimeT)], TimeT)
      -> pure (readValue "json" assign nx TimeT)

     ([(nx, StringT)], StringT)
      -> pure (readValuePool "json" assign nx StringT)

     (ns, StructT t)
      -> seaOfReadJsonObject assign t ns

     (ns, ArrayT t)
      -> seaOfReadJsonList t ns

     _
      -> Left (SeaInputTypeMismatch vtype vars)

------------------------------------------------------------------------

readValue :: Doc -> Assignment -> Text -> ValType -> Doc
readValue
 = readValueArg ""

readValuePool :: Doc -> Assignment -> Text -> ValType -> Doc
readValuePool
 = readValueArg "mempool, "

readValueArg :: Doc -> Doc -> Assignment -> Text -> ValType -> Doc
readValueArg arg fmt assign n vt
 = vsep
 [ seaOfValType vt <+> "value;"
 , "error = " <> fmt <> "_read_" <> baseOfValType vt <> " (" <> arg <> "&p, pe, &value);"
 , "if (error) return error;"
 , assign (pretty n) vt "value"
 ]

------------------------------------------------------------------------

seaOfReadJsonList :: ValType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonList vtype avars = do
  vars      <- traverse unArray avars
  value_sea <- seaOfReadJsonValue assignArray vtype vars
  pure $ vsep
    [ "if (*p++ != '[')"
    , "    return ierror_loc_format (p-1, p-1, \"array missing '['\");"
    , ""
    , "char term = *p;"
    , ""
    , "for (iint_t ix = 0; term != ']'; ix++) {"
    , indent 4 value_sea
    , "    "
    , "    term = *p++;"
    , "    if (term != ',' && term != ']')"
    , "        return ierror_loc_format (p-1, p-1, \"array separator ',' or terminator ']' not found\");"
    , "}"
    ]

unArray :: (Text, ValType) -> Either SeaError (Text, ValType)
unArray (n, ArrayT t) = Right (n, t)
unArray (n, t)        = Left (SeaInputTypeMismatch t [(n, t)])

------------------------------------------------------------------------

seaOfReadJsonObject :: Assignment -> StructType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonObject assign st@(StructType fs) vars
 = case vars of
    [(nx, UnitT)] | Map.null fs -> seaOfReadJsonUnit   assign nx
    _                           -> seaOfReadJsonStruct assign st vars

seaOfReadJsonUnit :: Assignment -> Text -> Either SeaError Doc
seaOfReadJsonUnit assign name = do
  pure $ vsep
    [ "if (*p++ != '{')"
    , "    return ierror_loc_format (p-1, p-1, \"unit missing '{'\");"
    , ""
    , "if (*p++ != '}')"
    , "    return ierror_loc_format (p-1, p-1, \"unit missing '}'\");"
    , ""
    , assign (pretty name) UnitT "iunit"
    ]

seaOfReadJsonStruct :: Assignment -> StructType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonStruct assign st@(StructType fields) vars = do
  let mismatch = SeaStructFieldsMismatch st vars
  mappings     <- maybe (Left mismatch) Right (mappingOfFields (Map.toList fields) vars)
  mappings_sea <- traverse (seaOfFieldMapping assign) mappings
  pure $ vsep
    [ "if (*p++ != '{')"
    , "    return ierror_loc_format (p-1, p-1, \"struct missing '{'\");"
    , ""
    , "for (;;) {"
    , "    if (*p++ != '\"')"
    , "        return ierror_loc_format (p-1, p-1, \"field name missing opening quote\");"
    , ""
    , indent 4 (vsep mappings_sea)
    , "    return ierror_loc_format (p-1, p-1, \"invalid field start\");"
    , "}"
    ]

seaOfFieldMapping :: Assignment -> FieldMapping -> Either SeaError Doc
seaOfFieldMapping assign (FieldMapping fname ftype vars) = do
  let needle = fname <> "\""
  field_sea <- seaOfReadJsonField assign ftype vars
  pure $ vsep
    [ "/* " <> pretty fname <> " */"
    , "if (" <> seaOfStringEq needle "p" Nothing <> ") {"
    , "    p += " <> int (sizeOfString needle) <> ";"
    , ""
    , indent 4 field_sea
    , ""
    , "    continue;"
    , "}"
    , ""
    ]

seaOfReadJsonField :: Assignment -> ValType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonField assign ftype vars = do
  value_sea <- seaOfReadJsonValue assign ftype vars
  pure $ vsep
    [ "if (*p++ != ':')"
    , "    return ierror_loc_format (p-1, p-1, \"field missing ':'\");"
    , ""
    , value_sea
    , ""
    , "char term = *p++;"
    , "if (term != ',' && term != '}')"
    , "    return ierror_loc_format (p-1, p-1, \"field separator ',' or terminator '}' not found\");"
    , ""
    , "if (term == '}')"
    , "    break;"
    ]

------------------------------------------------------------------------

data FieldMapping = FieldMapping {
    _fieldName :: Text
  , _fieldType :: ValType
  , _fieldVars :: [(Text, ValType)]
  } deriving (Eq, Ord, Show)

mappingOfFields :: [(StructField, ValType)] -> [(Text, ValType)] -> Maybe [FieldMapping]
mappingOfFields []     []  = pure []
mappingOfFields []     _   = Nothing
mappingOfFields (f:fs) vs0 = do
  (m,  vs1) <- mappingOfField  f  vs0
  ms        <- mappingOfFields fs vs1
  pure (m : ms)

mappingOfField :: (StructField, ValType) -> [(Text, ValType)] -> Maybe (FieldMapping, [(Text, ValType)])
mappingOfField (StructField fname, ftype) vars0 = do
  let go t (n, t')
       | t == t'   = Just (n, t)
       | otherwise = Nothing

  ns <- zipWithM go (meltType ftype) vars0

  let mapping = FieldMapping fname ftype ns
      vars1   = drop (length ns) vars0

  return (mapping, vars1)

------------------------------------------------------------------------

-- * Output

-- | A hack to tell whether or not strings should be quoted.
--   If in JSON, quote. If not, don't.
--   At any stage during output, if the elements should be JSON, pass @InJSON@,
--   e.g. when outputting arrays, we should specify that the elements be output
--        as JSON.
--
data IsInJSON = InJSON | NotInJSON

conditional :: Doc -> Doc -> Doc
conditional n body
 = vsep ["if (" <> n <> ")"
        , "{"
        , indent 4 body
        , "}"]

pair :: Doc -> Doc -> Doc
pair x y
 = vsep [ outputChar '['
        , x
        , outputChar ','
        , y
        , outputChar ']'
        ]

outputValue :: Doc -> [Doc] -> Doc
outputValue typ vals
 = vsep
 [ "error = psv_output_" <> typ <> " "
   <> "(fd, buffer, buffer_end, &buffer_ptr, " <> val <> ");"
 , outputDie
 ]
 where
  val = hcat (punctuate ", " vals)

forStmt :: Doc -> Doc -> Doc -> Doc
forStmt i n m
 = "for(iint_t" <+> i <+> "= 0," <+> n <+> "=" <+> m <> ";" <+> i <+> "<" <+> n <> "; ++" <> i <> ")"

outputChar :: Char -> Doc
outputChar x
 = outputValue "char" [seaOfChar x]

outputString :: Text -> Doc
outputString xs
 = vsep
 [ "if (buffer_end - buffer_ptr < " <> int rounded <> ") {"
 , "    error = psv_output_flush (fd, buffer, &buffer_ptr);"
 , indent 4 outputDie
 , "}"
 , vsep (fmap mkdoc swords)
 , "buffer_ptr += " <> int size <> ";"
 ]
 where
  swords = wordsOfString xs

  rounded  = length swords * 8
  size     = sum (fmap swSize swords)
  mkdoc sw = "*(uint64_t *)(buffer_ptr + " <> int (swOffset sw) <> ") = " <> swBits sw <> ";"

timeFmt :: Doc
timeFmt = "%04lld-%02lld-%02lldT%02lld:%02lld:%02lld"

outputDie :: Doc
outputDie = "if (error) return error;"

seaOfWriteFleetOutput :: PsvMode -> [SeaProgramState] -> Either SeaError Doc
seaOfWriteFleetOutput mode states = do
  write_sea <- traverse seaOfWriteProgramOutput states
  pure $ vsep
    [ "#line 1 \"write all outputs\""
    , "static ierror_msg_t psv_write_outputs"
    , "    ( int fd"
    , "    , char  *buffer"
    , "    , char  *buffer_end"
    , "    , char **buffer_ptr_ptr"
    , "    , const char *entity"
    , "    , size_t entity_size"
    , "    , ifleet_t *fleet )"
    , "{"
    , "    iint_t         chord_count = fleet->chord_count;"
    , "    const itime_t *chord_times = fleet->chord_times;"
    , "    ierror_msg_t   error;"
    , ""
    , "    char *buffer_ptr = *buffer_ptr_ptr;"
    , ""
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , indent 8 (seaOfChordTime mode)
    , indent 8 (vsep write_sea)
    , "    }"
    , ""
    , "    *buffer_ptr_ptr = buffer_ptr;"
    , ""
    , "    return 0;"
    , "}"
    ]

seaOfChordTime :: PsvMode -> Doc
seaOfChordTime = \case
  PsvSnapshot _ -> vsep
    [ "const char  *chord_time = \"\";"
    , "const size_t chord_size = 0;"
    ]
  PsvChords     -> vsep
    [ "iint_t c_year, c_month, c_day, c_hour, c_minute, c_second;"
    , "itime_to_gregorian (chord_times[chord_ix], &c_year, &c_month, &c_day, &c_hour, &c_minute, &c_second);"
    , ""
    , "const size_t chord_size = sizeof (\"|yyyy-mm-ddThh:mm:ssZ\");"
    , "char chord_time[chord_size];"
    , "snprintf (chord_time, chord_size, \"|" <> timeFmt <> "\", "
             <> "c_year, c_month, c_day, c_hour, c_minute, c_second);"
    ]

seaOfWriteProgramOutput :: SeaProgramState -> Either SeaError Doc
seaOfWriteProgramOutput state = do
  let ps    = "p" <> int (stateName state)
      stype = pretty (nameOfStateType state)
      pname = pretty (nameOfProgram state)

  let resumeables = fmap (\(n,_) -> ps <> "->" <> pretty (hasPrefix <> n) <+> "= ifalse;") (stateResumables state)
  outputs <- traverse (\(n,(t,ts)) -> seaOfWriteOutput ps n t ts 0) (stateOutputs state)

  pure $ vsep
    [ ""
    , "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
    , stype <+> "*" <> ps <+> "=" <+> "&fleet->" <> pname <> "[chord_ix];"
    , pname <+> "(" <> ps <> ");"
    , ps <> "->new_count = 0;"
    , vsep resumeables
    , ""
    , vsep outputs
    ]

seaOfWriteOutput :: Doc -> OutputName -> ValType -> [ValType] -> Int -> Either SeaError Doc
seaOfWriteOutput ps oname@(OutputName name) otype0 ts0 ixStart
  = let members = List.take (length ts0) (fmap (\ix -> ps <> "->" <> seaOfNameIx name ix) [ixStart..])
    in case otype0 of
         -- Top-level Sum is a special case, to avoid allocating and printing if
         -- the whole computation is an error (e.g. tombstone)
         SumT ErrorT otype1
          | (ErrorT : ts1) <- ts0
          , (ne     : _)   <- members
          -> do (m, body, _, _) <- seaOfOutput NotInJSON ps (ixStart + 1)
                                               oname Map.empty otype1 ts1 id
                let body'        = seaOfOutputCond m body
                let body''       = go body'
                pure $ conditional (ne <> " == ierror_not_an_error") body''
         _
          -> do (m, body, _, _) <- seaOfOutput NotInJSON ps ixStart
                                               oname Map.empty otype0 ts0 id
                let body'        = seaOfOutputCond m body
                return $ go body'

  where
    before = vsep [ outputValue  "string" ["entity", "entity_size"]
                  , outputString ("|" <> name <> "|") ]

    after  = vsep [ outputValue  "string" ["chord_time", "chord_size"]
                  , outputChar   '\n' ]

    go str = vsep [before, str, after]

--------------------------------------------------------------------------------

-- | A mapping of C name prefixes in use to the number of times they are used.
--   e.g. if @x_0@ and @x_1@ have been used, the environment must contain @(x, 2)@
--
type NameEnv = Map String Int

newName :: Doc -> NameEnv -> (Doc, NameEnv)
newName doc env
  = let n = show doc
    in  case Map.lookup n env of
          Nothing
            -> (pretty (0 :: Int), Map.insert n 1 env)
          Just i
            -> (pretty i, Map.insert n (i + 1) env)

seaOfOutput
  :: IsInJSON                      -- ^ Indicates whether to quote strings
  -> Doc                           -- ^ Struct of values to output
  -> Int                           -- ^ Current index into the struct of values
  -> OutputName                    -- ^ Use the output name as seed for generated C names
  -> NameEnv                       -- ^ C names in use
  -> ValType                       -- ^ Output type
  -> [ValType]                     -- ^ Types of arguments
  -> (Doc -> Doc)                  -- ^ Transformation to be applied to this struct member, e.g. index
  -> Either SeaError ( Maybe Doc   -- Top level condition for the output statement
                     , Doc         -- The output statement
                     , Int         -- Where it's up to
                     , [ValType] ) -- Unconsumed arguments
seaOfOutput isJSON struct structIndex outName@(OutputName name) env outType argTypes transform
 = let prefixi         = pretty name <> "_" <> pretty structIndex <> "_i"
       (suffixi, env'')= newName prefixi env
       counter         = prefixi <> suffixi

       prefixn         = pretty name <> "_" <> pretty structIndex <> "_n"
       (suffixn, env') = newName prefixn env''
       countLimit      = prefixn <> suffixn

       arrayIndex t x  = seaOfArrayIndex x counter t

   in case outType of
       ArrayT te
        | tes@(arg0:_) <- meltType te
        , (arr  : _)   <- members
        -> do (mcond, body, ix, ts1) <- seaOfOutput InJSON struct structIndex
                                                    outName env' te tes
                                                    (arrayIndex arg0 . transform)

              -- End the body with a comma, if applicable
              let body' = seaOfOutputCond mcond
                        $ vsep [seaOfOutputArraySep counter, body]

              -- Array of arrays is allowed, so we apply the transform here
              let arr'  = transform arr

              -- Wrap the body in a for loop
              let numElems  = arrayCount arr'
              body''       <- seaOfOutputArray body' numElems counter countLimit

              return (Nothing, body'', ix, ts1)


       MapT tk tv
        | tks@(argk0 : _) <- meltType tk
        , tvs@(argv0 : _) <- meltType tv
        , (arr : _)       <- members
        -> do (mcondk, bk, ixk, _)  <- seaOfOutput InJSON struct structIndex
                                                   outName env' tk tks
                                                   (arrayIndex argk0 . transform)
              (mcondv, bv, ixv, ts) <- seaOfOutput InJSON struct ixk
                                                   outName env' tv tvs
                                                   (arrayIndex argv0 . transform)

              let p  = pair bk bv
              let p' = seaOfOutputCond mcondk
                     $ seaOfOutputCond mcondv
                     $ vsep [seaOfOutputArraySep counter, p]

              let numElems  = arrayCount arr
              body         <- seaOfOutputArray p' numElems counter countLimit
              return (Nothing, body, ixv, ts)


       PairT ta tb
        | tas <- meltType ta
        , tbs <- meltType tb
        -> do (mcondk, ba, ixa, _)  <- seaOfOutput InJSON struct structIndex
                                                   outName env' ta tas transform
              (mcondv, bb, ixb, ts) <- seaOfOutput InJSON struct ixa
                                                   outName env' tb tbs transform

              let p  = pair ba bb
              let p' = seaOfOutputCond mcondk
                     $ seaOfOutputCond mcondv
                     $ p

              return (Nothing, p', ixb, ts)

       -- Conditionals
       OptionT otype1
        | (BoolT : ts1) <- argTypes
        , (nb    : _)   <- members
        -> do (mcond, body, ix, ts) <- seaOfOutput isJSON struct (structIndex + 1)
                                                   outName env' otype1 ts1 transform

              let body' = seaOfOutputCond mcond body
              let nb'   = transform nb
              pure (Just nb', body', ix, ts)

       SumT ErrorT otype1
        | (ErrorT : ts1) <- argTypes
        , (ne     : _)   <- members
        -> do (mcond, body, ix, ts) <- seaOfOutput isJSON struct (structIndex + 1)
                                                   outName env' otype1 ts1 transform

              let body' = seaOfOutputCond mcond body
              let ne'   = transform ne
              pure (Just (ne' <> " == ierror_not_an_error"), body', ix, ts)

       -- Base
       _
        | (t  : ts) <- argTypes
        , (mx : _)  <- members
        , mx'       <- transform mx
        -> do d <- seaOfOutputBase' isJSON t mx'
              pure (Nothing, d, structIndex + 1, ts)

       _ -> Left unsupported

  where
   mismatch    = SeaOutputTypeMismatch    outName outType argTypes
   unsupported = SeaUnsupportedOutputType outType

   members    = List.take (length argTypes)
              $ fmap (\ix -> struct <> "->" <> seaOfNameIx name ix) [structIndex..]

   arrayCount x
    = "(" <> x <> ")" <> "->count"

   seaOfOutputBase' b
    = seaOfOutputBase b mismatch

   seaOfOutputArraySep c
     = conditional (c <+> "> 0") seaOfOutputSep

   seaOfOutputSep
     = outputChar ','

--------------------------------------------------------------------------------

-- | Output an array with pre-defined bodies
seaOfOutputArray :: Applicative f => Doc -> Doc -> Doc -> Doc -> f Doc
seaOfOutputArray body numElems counter countLimit
 = pure (vsep [ outputChar '['
              , forStmt counter countLimit numElems
              , "{"
              , indent 4 body
              , "}"
              , outputChar ']'
              ]
        )

-- | Output an if statement
seaOfOutputCond :: Maybe Doc -> Doc -> Doc
seaOfOutputCond mcond body
  = case mcond of
      Nothing
        -> body
      Just cond
        -> conditional cond body

-- | Output single types
seaOfOutputBase :: IsInJSON -> SeaError -> ValType -> Doc -> Either SeaError Doc
seaOfOutputBase quoteStrings err t val
 = case t of
     BoolT
      -> pure
       $ vsep
           [ "if (" <> val <> ") {"
           , indent 4 $ outputString "true"
           , "} else {"
           , indent 4 $ outputString "false"
           , "}"
           ]
     IntT
      -> pure $ outputValue "int" [val]
     DoubleT
      -> pure $ outputValue "double" [val]
     StringT
      -> pure $ quotedOutput quoteStrings (outputValue "string" [val, "strlen(" <> val <> ")"])
     TimeT
      -> pure $ quotedOutput quoteStrings (outputValue "time" [val])

     _ -> Left err

------------------------------------------------------------------------

quotedOutput :: IsInJSON -> Doc -> Doc
quotedOutput NotInJSON out = out
quotedOutput InJSON    out = vsep [outputChar '"', out, outputChar '"']

sizeOfString :: Text -> Int
sizeOfString = B.length . T.encodeUtf8

seaOfStringEq :: Text -> Doc -> Maybe Doc -> Doc
seaOfStringEq str ptr msize
 | Just size <- msize = align (vsep [szdoc size, cmpdoc])
 | otherwise          = align cmpdoc
 where
   nbytes = length bytes
   bytes  = B.unpack (T.encodeUtf8 str)

   szdoc size = size <+> "==" <+> int nbytes <+> "&&"
   cmpdoc     = seaOfBytesEq bytes ptr

seaOfBytesEq :: [Word8] -> Doc -> Doc
seaOfBytesEq bs ptr
 = vsep . punctuate " &&" . fmap go $ wordsOfBytes bs
 where
   go (StringWord off _ mask bits)
    = "(*(uint64_t *)(" <> ptr <+> "+" <+> int off <> ") &" <+> mask <> ") ==" <+> bits

------------------------------------------------------------------------

data StringWord = StringWord {
    swOffset :: Int
  , swSize   :: Int
  , swMask   :: Doc
  , swBits   :: Doc
  }

wordsOfString :: Text -> [StringWord]
wordsOfString
 = wordsOfBytes . B.unpack . T.encodeUtf8

wordsOfBytes :: [Word8] -> [StringWord]
wordsOfBytes bs
 = reverse (wordsOfBytes' bs 0 [])

wordsOfBytes' :: [Word8] -> Int -> [StringWord] -> [StringWord]
wordsOfBytes' [] _   acc = acc
wordsOfBytes' bs off acc
 = wordsOfBytes' remains (off + 8) (sw : acc)
 where
  sw = StringWord { swOffset = off, swSize = nbytes, swMask = mask, swBits = bits }

  (bytes, remains) = splitAt 8 bs

  nbytes = length bytes

  nzeros = 8 - nbytes
  zeros  = List.replicate nzeros 0x00

  mask = text $ "0x" <> concatMap (printf "%02X") (zeros <> List.replicate nbytes 0xff)
  bits = text $ "0x" <> concatMap (printf "%02X") (zeros <> reverse bytes)

------------------------------------------------------------------------

lookupTombstones :: PsvConfig -> SeaProgramState -> Set Text
lookupTombstones config state =
  fromMaybe Set.empty (Map.lookup (stateAttribute state) (psvTombstones config))

------------------------------------------------------------------------
-- Should be in P?

last :: [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x:) <$> init xs
