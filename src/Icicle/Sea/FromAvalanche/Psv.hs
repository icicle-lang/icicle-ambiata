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

import           Icicle.Data (Attribute(..), DateTime)

import           Icicle.Internal.Pretty
import qualified Icicle.Internal.Pretty as Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base (seaOfAttributeDesc, seaOfDate)
import           Icicle.Sea.FromAvalanche.Base (seaOfNameIx, seaOfEscaped)
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Program (seaOfXValue)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P

import           Text.Printf (printf)


------------------------------------------------------------------------

data PsvMode
  = PsvSnapshot DateTime
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
 = let constDate = "const " <> seaOfValType DateTimeT
   in vsep
      [ "#line 1 \"fleet state\""
      , "struct ifleet {"
      , indent 4 (defOfVar' 1 "imempool_t" "mempool")         <> ";"
      , indent 4 (defOfVar  0 IntT         "max_chord_count") <> ";"
      , indent 4 (defOfVar  0 IntT         "chord_count")     <> ";"
      , indent 4 (defOfVar' 1 constDate    "chord_dates")     <> ";"
      , indent 4 (vsep (fmap defOfProgramState states))
      , "};"
      ]

defOfProgramState :: SeaProgramState -> Doc
defOfProgramState state
 = defOfVar' 1 (pretty (nameOfStateType state))
               (pretty (nameOfProgram state)) <> ";"
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
  DateTimeT -> False
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
 , "static psv_error_t psv_configure_fleet (const char *entity, size_t entity_size, const ichord_t **chord, ifleet_t *fleet)"
 , "{"
 , "    iint_t max_chord_count = fleet->max_chord_count;"
 , ""
 , "    iint_t         chord_count;"
 , "    const idate_t *chord_dates;"
 , ""
 , case mode of
     PsvSnapshot date -> indent 4 (seaOfChordDates [date])
     PsvChords        -> indent 4 seaOfChordScan
 , ""
 , "    if (chord_count > max_chord_count) {"
 , "        char msg[1024] = {0};"
 , ""
 , "        snprintf ( msg, sizeof (msg)"
 , "                 , \"exceeded maximum number of chords per entity (chord_count = %lld, max_chord_count = %lld)\""
 , "                 , chord_count"
 , "                 , max_chord_count);"
 , ""
 , "        return psv_alloc_error (msg, 0, 0);"
 , "    }"
 , ""
 , "    fleet->chord_count = chord_count;"
 , "    fleet->chord_dates = chord_dates;"
 , ""
 , indent 4 (vsep (fmap defOfState states))
 , ""
 , "    for (iint_t ix = 0; ix < chord_count; ix++) {"
 , "        idate_t chord_date = chord_dates[ix];"
 , ""
 , indent 8 (vsep (fmap seaOfAssignDate states))
 , "    }"
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

seaOfAssignDate :: SeaProgramState -> Doc
seaOfAssignDate state
 = let pdate = "p" <> pretty (stateName state) <> "[ix]." <> pretty (stateDateVar state)
   in pdate <+> "=" <+> "chord_date;"

seaOfChordDates :: [DateTime] -> Doc
seaOfChordDates dates
 = vsep
 [ "static const idate_t entity_dates[] = { " <> hcat (punctuate ", " (fmap seaOfDate dates)) <> " };"
 , ""
 , "chord_count = " <> int (length dates) <> ";"
 , "chord_dates = entity_dates;"
 ]

seaOfChordScan :: Doc
seaOfChordScan
 = "*chord = ichord_scan (*chord, entity, entity_size, &chord_count, &chord_dates);"

------------------------------------------------------------------------

seaOfReadAnyFact :: PsvConfig -> [SeaProgramState] -> Either SeaError Doc
seaOfReadAnyFact config states = do
  let tss = fmap (lookupTombstones config) states
  readStates_sea <- zipWithM seaOfReadFact states tss
  pure $ vsep
    [ vsep readStates_sea
    , ""
    , "#line 1 \"read any fact\""
    , "static psv_error_t psv_read_fact"
    , "  ( const char   *attrib"
    , "  , const size_t  attrib_size"
    , "  , const char   *value"
    , "  , const size_t  value_size"
    , "  , idate_t       date"
    , "  , ifleet_t     *fleet )"
    , "{"
    , "    ibool_t        ignore_date = itrue;"
    , "    iint_t         chord_count = fleet->chord_count;"
    , "    const idate_t *chord_dates = fleet->chord_dates;"
    , ""
    , "    /* ignore this date if it comes after all the chord dates */"
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , "        if (chord_dates[chord_ix] > date) {"
    , "            ignore_date = ifalse;"
    , "            break;"
    , "        }"
    , "    }"
    , ""
    , "    if (ignore_date) return 0;"
    , ""
    , indent 4 (vsep (fmap seaOfReadNamedFact states))
    , "    return 0;"
    , "}"
    ]

seaOfReadNamedFact :: SeaProgramState -> Doc
seaOfReadNamedFact state
 = let attrib = getAttribute (stateAttribute state)
       fun    = pretty (nameOfReadFact state)
       pname  = pretty (nameOfProgram state)
   in vsep
      [ "/* " <> pretty attrib <> " */"
      , "if (" <> seaOfStringEq attrib "attrib" (Just "attrib_size") <> ") {"
      , "    return " <> fun <> " (value, value_size, date, fleet->mempool, chord_count, fleet->" <> pname <> ");"
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
    , "static psv_error_t INLINE"
        <+> pretty (nameOfReadFact state) <+> "("
        <> "const char *value_ptr, const size_t value_size, idate_t date, "
        <> "imempool_t *mempool, iint_t chord_count, "
        <> pretty (nameOfStateType state) <+> "*programs)"
    , "{"
    , "    psv_error_t error;"
    , ""
    , "    char *p  = (char *) value_ptr;"
    , "    char *pe = (char *) value_ptr + value_size;"
    , ""
    , "    ibool_t " <> pretty (inputSumBool input) <> ";"
    , indent 4 . vsep . fmap seaOfDefineInput $ inputVars input
    , ""
    , "    " <> align (seaOfReadTombstone input (Set.toList tombstones)) <> "{"
    , "        " <> pretty (inputSumBool input) <> " = itrue;"
    , ""
    , indent 8 readInput
    , "    }"
    , ""
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , "        " <> pretty (nameOfStateType state) <+> "*program = &programs[chord_ix];"
    , ""
    , "        /* don't read values after the chord date */"
    , "        if (date > program->" <> pretty (stateDateVar state) <> ")"
    , "            continue;"
    , ""
    , "        iint_t new_count = program->new_count;"
    , ""
    , "        program->" <> pretty (inputSumBool  input) <> "[new_count] = " <> pretty (inputSumBool input) <> ";"
    , "        program->" <> pretty (inputSumError input) <> "[new_count] = ierror_tombstone;"
    , indent 8 . vsep . fmap seaOfAssignInput $ inputVars input
    , "        program->" <> pretty (inputDate     input) <> "[new_count] = date;"
    , ""
    , "        new_count++;"
    , ""
    , "        if (new_count == psv_max_row_count) {"
    , "             " <> pretty (nameOfProgram state) <> " (program);"
    , "             new_count = 0;"
    , "        } else if (new_count > psv_max_row_count) {"
    , "             return psv_alloc_error (\"" <> pretty (nameOfReadFact state) <> ": new_count > max_count\", 0, 0);"
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
         <> "    " <> pretty (inputSumBool input) <> " = ifalse;" <> line
         <> "} else " <> seaOfReadTombstone input ts

------------------------------------------------------------------------

data CheckedInput = CheckedInput {
    inputSumBool  :: Text
  , inputSumError :: Text
  , inputDate     :: Text
  , inputType     :: ValType
  , inputVars     :: [(Text, ValType)]
  } deriving (Eq, Ord, Show)

checkInputType :: SeaProgramState -> Either SeaError CheckedInput
checkInputType state
 = case stateInputType state of
     PairT (SumT ErrorT t) DateTimeT
      | (sumBool,  BoolT)  : xs0 <- stateInputVars state
      , (sumError, ErrorT) : xs1 <- xs0
      , Just vars                <- init xs1
      , Just (date, DateTimeT)   <- last xs1
      -> Right CheckedInput {
             inputSumBool  = newPrefix <> sumBool
           , inputSumError = newPrefix <> sumError
           , inputDate     = newPrefix <> date
           , inputType     = t
           , inputVars     = fmap (first (newPrefix <>)) vars
           }

     t
      -> Left (SeaUnsupportedInputType t)

seaOfReadInput :: CheckedInput -> Either SeaError Doc
seaOfReadInput input
 = case (inputVars input, inputType input) of
    ([(nx, BoolT)], BoolT)
     -> pure $ vsep
        [ "error = psv_read_bool (&p, pe, &" <> pretty nx <> ");"
        , "if (error) return error;"
        ]

    ([(nx, DoubleT)], DoubleT)
     -> pure $ vsep
        [ "error = psv_read_double (&p, pe, &" <> pretty nx <> ");"
        , "if (error) return error;"
        ]

    ([(nx, IntT)], IntT)
     -> pure $ vsep
        [ "error = psv_read_int (&p, pe, &" <> pretty nx <> ");"
        , "if (error) return error;"
        ]

    ([(nx, DateTimeT)], DateTimeT)
     -> pure $ vsep
        [ "error = psv_read_date (value_ptr, value_size, &" <> pretty nx <> ");"
        , "if (error) return error;"
        ]

    ([(nx, StringT)], StringT)
     -> pure $ vsep
        [ "error = psv_read_string (mempool, &p, pe, &" <> pretty nx <> ");"
        , "if (error) return error;"
        ]

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
 = let readValueArg arg n vt suf = vsep
         [ seaOfValType vtype <+> "value;"
         , "error = psv_read_" <> suf <> " (" <> arg <> "&p, pe, &value);"
         , "if (error) return error;"
         , assign (pretty n) vt "value"
         ]

       readValue     = readValueArg ""
       readValuePool = readValueArg "mempool, "

   in case (vars, vtype) of
       ([(nb, BoolT), nx], OptionT t) -> do
         val_sea <- seaOfReadJsonValue assign t [nx]
         pure $ vsep
           [ "ibool_t is_null;"
           , "error = psv_try_read_json_null (&p, pe, &is_null);"
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
        -> pure (readValue nx BoolT "json_bool")

       ([(nx, IntT)], IntT)
        -> pure (readValue nx IntT "int")

       ([(nx, DoubleT)], DoubleT)
        -> pure (readValue nx DoubleT "double")

       ([(nx, DateTimeT)], DateTimeT)
        -> pure (readValue nx DateTimeT "json_date")

       ([(nx, StringT)], StringT)
        -> pure (readValuePool nx StringT "json_string")

       (ns, StructT t)
        -> seaOfReadJsonObject assign t ns

       (ns, ArrayT t)
        -> seaOfReadJsonList t ns

       _
        -> Left (SeaInputTypeMismatch vtype vars)

------------------------------------------------------------------------

seaOfReadJsonList :: ValType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadJsonList vtype avars = do
  vars      <- traverse unArray avars
  value_sea <- seaOfReadJsonValue assignArray vtype vars
  pure $ vsep
    [ "if (*p++ != '[')"
    , "    return psv_alloc_error (\"missing '['\",  p, pe - p);"
    , ""
    , "char term = *p;"
    , ""
    , "for (iint_t ix = 0; term != ']'; ix++) {"
    , indent 4 value_sea
    , "    "
    , "    term = *p++;"
    , "    if (term != ',' && term != ']')"
    , "        return psv_alloc_error (\"terminator ',' or ']' not found\", p, pe - p);"
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
    , "    return psv_alloc_error (\"missing {\",  p, pe - p);"
    , ""
    , "if (*p++ != '}')"
    , "    return psv_alloc_error (\"missing }\",  p, pe - p);"
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
    , "    return psv_alloc_error (\"missing {\",  p, pe - p);"
    , ""
    , "for (;;) {"
    , "    if (*p++ != '\"')"
    , "        return psv_alloc_error (\"missing \\\"\", p, pe - p);"
    , ""
    , indent 4 (vsep mappings_sea)
    , "    return psv_alloc_error (\"invalid field start\", p, pe - p);"
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
    , "    return psv_alloc_error (\"missing ':'\",  p, pe - p);"
    , ""
    , value_sea
    , ""
    , "char term = *p++;"
    , "if (term != ',' && term != '}')"
    , "    return psv_alloc_error (\"terminator ',' or '}' not found\", p, pe - p);"
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

cond :: Doc -> Doc -> Doc
cond n body
 = vsep ["if (" <> n <> ")"
        , "{"
        , indent 4 body
        , "}"]

pair :: Doc -> Doc -> Doc
pair x y
 = vsep [ outputChar "["
        , x
        , outputChar ","
        , y
        , outputChar "]"
        ]

iprintf :: Doc -> Doc -> Doc
iprintf fmt val
 = vsep
 [ "psv_output_error = psv_output_printf (fd, psv_output_buf, psv_output_buf_end, &psv_output_buf_ptr,\""
   <> fmt <> "\"," <+> val <> ");"
 , outputDie
 ]

forStmt :: Doc -> Doc -> Doc -> Doc
forStmt i n m
 = "for(iint_t" <+> i <+> "= 0," <+> n <+> "=" <+> m <> ";" <+> i <+> "<" <+> n <> "; ++" <> i <> ")"

outputChar :: Doc -> Doc
outputChar x
 = iprintf "%c" ("'" <> x <> "'")

dateFmt :: Doc
dateFmt = "%lld-%02lld-%02lldT%02lld:%02lld:%02lld"

outputDie :: Doc
outputDie = "if (psv_output_error) return psv_output_error;"

seaOfWriteFleetOutput :: PsvMode -> [SeaProgramState] -> Either SeaError Doc
seaOfWriteFleetOutput mode states = do
  write_sea <- traverse seaOfWriteProgramOutput states
  pure $ vsep
    [ "#line 1 \"write all outputs\""
    , "static psv_error_t psv_write_outputs (int fd, const char *entity, ifleet_t *fleet)"
    , "{"
    , "    iint_t         chord_count = fleet->chord_count;"
    , "    const idate_t *chord_dates = fleet->chord_dates;"
    , "    psv_error_t    psv_output_error;"
    , "    char           psv_output_buf[psv_output_buf_size];"
    , "    char          *psv_output_buf_end = psv_output_buf + psv_output_buf_size - 1;"
    , "    char          *psv_output_buf_ptr = psv_output_buf;"
    , ""
    , "    bzero (psv_output_buf, psv_output_buf_size);"
    , ""
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , indent 8 (seaOfChordDate mode)
    , ""
    , indent 8 (vsep write_sea)
    , "    }"
    , ""
    , "    psv_output_error = psv_output_flush (fd, psv_output_buf, psv_output_buf_ptr);"
    , outputDie
    , ""
    , "    return 0;"
    , "}"
    ]

seaOfChordDate :: PsvMode -> Doc
seaOfChordDate = \case
  PsvSnapshot _ -> "const char *chord_date = \"\";"
  PsvChords     -> vsep
    [ "iint_t c_year, c_month, c_day, c_hour, c_minute, c_second;"
    , "idate_to_gregorian (chord_dates[chord_ix], &c_year, &c_month, &c_day, &c_hour, &c_minute, &c_second);"
    , ""
    , "const size_t chord_size = sizeof (\"|yyyy-mm-ddThh:mm:ssZ\");"
    , "char chord_date[chord_size];"
    , "snprintf (chord_date, chord_size, \"|" <> dateFmt <> "\", "
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
          | (BoolT : ErrorT : ts1) <- ts0
          , (nb    : _      : _)   <- members
          -> do (body, _, _) <- seaOfOutput False ps oname otype1 ts1 (ixStart+2) (const id)
                let body'     = go body
                pure $ cond nb body'
         _
          -> do (body, _, _) <- seaOfOutput False ps oname otype0 ts0 ixStart (const id)
                return $ go body

  where
    attrib = seaOfEscaped name
    before = iprintf ("%s|" <> attrib <> "|") "entity"
    after  = iprintf "%s\\n"                 "chord_date"

    decldt = "iint_t v_year, v_month, v_day, v_hour, v_minute, v_second;" :: Doc

    go str
      =  vsep
      $  [ if hasDateTime otype0 then decldt else mempty ]
      <> [ before
         , str
         , after
         ]

    hasDateTime DateTimeT   = True
    hasDateTime (ArrayT t)  = hasDateTime t
    hasDateTime (BufT _ t)  = hasDateTime t
    hasDateTime (OptionT t) = hasDateTime t
    hasDateTime (MapT k v)  = hasDateTime k || hasDateTime v
    hasDateTime (PairT a b) = hasDateTime a || hasDateTime b
    hasDateTime (SumT a b)  = hasDateTime a || hasDateTime b
    hasDateTime (StructT m) = any hasDateTime $ Map.elems $ getStructType m
    hasDateTime _           = False

seaOfOutput
  :: Bool                          -- ^ whether to quote date (MASSIVE HACK)
  -> Doc                           -- ^ struct
  -> OutputName
  -> ValType                       -- ^ output type
  -> [ValType]                     -- ^ types of arguments
  -> Int                           -- ^ struct index
  -> (ValType -> Doc -> Doc)       -- ^ apply this to struct members
  -> Either SeaError ( Doc         -- output statements for consumed arguments
                     , Int         -- where it's up to
                     , [ValType] ) -- unconsumed arguments
seaOfOutput q ps oname@(OutputName name) otype0 ts0 ixStart transform
  = case otype0 of
      ArrayT te
       | tes        <- meltType te
       , (arr : _)  <- members
       -> do (body, ix, _) <- seaOfOutput True ps oname te tes ixStart arrayIndex
             body'         <- seaOfOutputArray body arr
             let ts1        = List.drop (length tes) ts0
             return (body', ix, ts1)

      MapT tk tv
       | tks        <- meltType tk
       , tvs        <- meltType tv
       , length ts0 == length tks + length tvs
       , (arr: _)   <- members
       -> do (bk, ixk, _)  <- seaOfOutput True ps oname tk tks ixStart arrayIndex
             (bv, ixv, ts) <- seaOfOutput True ps oname tv tvs ixk     arrayIndex
             body'         <- seaOfOutputArray (pair bk bv) arr
             return (body', ixv, ts)


      OptionT otype1
       | (BoolT : ts1) <- ts0
       , (nb    : _)   <- members
       -> do (body, ix, ts) <- seaOfOutput q ps oname otype1 ts1 (ixStart+1) transform
             pure (cond nb body, ix, ts)

      PairT ta tb
       | tas <- meltType ta
       , tbs <- meltType tb
       -> do (ba, ixa, _)  <- seaOfOutput True ps oname ta tas ixStart transform
             (bb, ixb, ts) <- seaOfOutput True ps oname tb tbs ixa     transform
             return (pair ba bb, ixb, ts)

      SumT ErrorT otype1
       | (BoolT : ErrorT : ts1) <- ts0
       , (nb    : _      : _)   <- members
       -> do (body, ix, ts) <- seaOfOutput False ps oname otype1 ts1 (ixStart+2) transform
             pure (cond nb body, ix, ts)
      _
       | (t  : ts) <- ts0
       , (mx : _)  <- members
       , mx'  <- transform t mx
       -> do d <- seaOfOutputBase' q t mx'
             pure (d, ixStart + 1, ts)

      _ -> Left unsupported

  where
   mismatch    = SeaOutputTypeMismatch oname otype0 ts0
   unsupported = SeaUnsupportedOutputType otype0

   members = List.take (length ts0) (fmap (\ix -> ps <> "->" <> seaOfNameIx name ix) [ixStart..])
   counter = pretty name <> "_i"

   arrayIndex t x
    = seaOfArrayIndex x counter t

   seaOfOutputBase' b
    = seaOfOutputBase b mismatch

   -- Output an array with pre-defined bodies
   seaOfOutputArray body array
    = let namePrefix = pretty name
          i          = namePrefix <> "_i"
          limit      = namePrefix <> "_n"
          numElems   = array <> "->count"
      in  pure
           (vsep [ outputChar "["
                 , forStmt i limit numElems
                 , "{"
                 , indent 4
                     $ cond (i <+> "> 0")
                            (outputChar ",")
                 , indent 4 body
                 , "}"
                 , outputChar "]"
                 ]
           )

-- | Output single types
seaOfOutputBase :: Bool -> SeaError -> ValType -> Doc -> Either SeaError Doc
seaOfOutputBase quoteStrings err t val
 = case t of
     BoolT
      -> pure
       $ vsep
           [ "if (" <> val <> ") {"
           , indent 4 $ iprintf "%s" "\"true\""
           , "} else {"
           , indent 4 $ iprintf "%s" "\"false\""
           , "}"
           ]
     IntT
      -> pure $ vsep [iprintf "%lld" val]
     DoubleT
      -> pure $ vsep [iprintf "%.16g" val]
     StringT
      -> pure $ vsep [iprintf (quotedFormat quoteStrings "%s") val]
     DateTimeT
      -> pure
       $ vsep [ "idate_to_gregorian (" <> val <> ", &v_year, &v_month, &v_day, &v_hour, &v_minute, &v_second);"
              , iprintf
                  (quotedFormat quoteStrings dateFmt) -- uuggghhhh
                  "v_year, v_month, v_day, v_hour, v_minute, v_second"
              ]

     _ -> Left err

quotedFormat :: Bool -> Doc -> Doc
quotedFormat False fmt = fmt
quotedFormat True  fmt = "\\\"" <> fmt <> "\\\""

------------------------------------------------------------------------

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
 = vsep . punctuate " &&" . reverse $ seaOfBytesEq' bs 0 ptr []

seaOfBytesEq' :: [Word8] -> Int -> Doc -> [Doc] -> [Doc]
seaOfBytesEq' [] _   _   acc = acc
seaOfBytesEq' bs off ptr acc
 = seaOfBytesEq' remains (off + 8) ptr (doc : acc)
 where
   (bytes, remains) = splitAt 8 bs

   nbytes = length bytes

   nzeros = 8 - nbytes
   zeros  = List.replicate nzeros 0x00

   mask = text $ "0x" <> concatMap (printf "%02X") (zeros <> List.replicate nbytes 0xff)
   bits = text $ "0x" <> concatMap (printf "%02X") (zeros <> reverse bytes)

   doc = "(*(uint64_t *)(" <> ptr <+> "+" <+> int off <> ") &" <+> mask <> ") ==" <+> bits

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
