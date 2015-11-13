{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.Psv (
    seaOfPsvDriver
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Avalanche.Prim.Flat (meltType)

import           Icicle.Common.Base (OutputName(..))
import           Icicle.Common.Type (ValType(..), StructType(..), StructField(..))

import           Icicle.Data (Attribute(..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P


------------------------------------------------------------------------

seaOfPsvDriver :: [SeaProgramState] -> Either SeaError Doc
seaOfPsvDriver states = do
  let struct_sea = seaOfFleetState states
      alloc_sea  = seaOfAllocFleet states
  read_sea  <- seaOfReadAnyFact    states
  write_sea <- seaOfWriteFleetOutput   states
  pure $ vsep
    [ struct_sea
    , ""
    , alloc_sea
    , ""
    , read_sea
    , ""
    , write_sea
    ]

------------------------------------------------------------------------

seaOfFleetState :: [SeaProgramState] -> Doc
seaOfFleetState states
 = vsep
 [ "#line 1 \"fleet state\""
 , "struct ifleet {"
 , "    idate_t    snapshot_date;"
 , indent 4 (vsep (fmap defOfProgramState states))
 , "};"
 ]

defOfProgramState :: SeaProgramState -> Doc
defOfProgramState state
 =   pretty (nameOfStateType state)
 <+> pretty (nameOfProgram   state) <> ";"
 <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

------------------------------------------------------------------------

seaOfAllocFleet :: [SeaProgramState] -> Doc
seaOfAllocFleet states
 = vsep
 [ "#line 1 \"allocate fleet state\""
 , "static ifleet_t * psv_alloc_fleet ()"
 , "{"
 , "    idate_t date = idate_from_gregorian (2015, 1, 1, 0, 0, 0);"
 , ""
 , "    ifleet_t *fleet = calloc (1, sizeof (ifleet_t));"
 , "    fleet->snapshot_date = date;"
 , ""
 , indent 4 (vsep (fmap seaOfAllocProgram states))
 , "    return fleet;"
 , "}"
 ]

seaOfAllocProgram :: SeaProgramState -> Doc
seaOfAllocProgram state
 = let ps        = "fleet->" <> pretty (nameOfProgram state) <> "."
       go (n, t) = ps <> pretty (newPrefix <> n) <> " = "
                <> "calloc (psv_max_row_count, sizeof (" <> noPadSeaOfValType t <> "));"

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , ps <> "gen_date = date;"
           , vsep (fmap go (stateInputVars state))
           , ""
           ]

------------------------------------------------------------------------

seaOfReadAnyFact :: [SeaProgramState] -> Either SeaError Doc
seaOfReadAnyFact states = do
  readStates_sea <- traverse seaOfReadFact states
  pure $ vsep
    [ vsep readStates_sea
    , ""
    , "#line 1 \"read any fact\""
    , "static psv_error_t psv_read_fact"
    , "  ( ifleet_t     *fleet"
    , "  , const char   *attrib"
    , "  , const size_t  attrib_size"
    , "  , const char   *value"
    , "  , const size_t  value_size"
    , "  , idate_t       date )"
    , "{"
    , "    /* don't read values after the snapshot date */"
    , "    if (date > fleet->snapshot_date)"
    , "        return 0;"
    , ""
    , "    const size_t attrib0_size = attrib_size + 1;"
    , indent 4 (vsep (fmap seaOfReadNamedFact states))
    , ""
    , "    return 0;"
    , "}"
    ]

seaOfReadNamedFact :: SeaProgramState -> Doc
seaOfReadNamedFact state
 = vsep
 [ ""
 , "if (sizeof (" <> seaOfAttribute state <> ") == attrib0_size &&"
 , "    memcmp (" <> seaOfAttribute state <> ", attrib, attrib_size) == 0) {"
 , "    return " <> pretty (nameOfReadFact state)
                 <> " (&fleet->" <> pretty (nameOfProgram state) <> ", value, value_size, date);"
 , "}"
 ]

------------------------------------------------------------------------

nameOfReadFact :: SeaProgramState -> Text
nameOfReadFact state = T.pack ("psv_read_fact_" <> show (stateName state))

seaOfReadFact :: SeaProgramState -> Either SeaError Doc
seaOfReadFact state = do
  input     <- checkInputType state
  readInput <- seaOfReadInput input
  pure $ vsep
    [ "#line 1 \"read fact" <+> seaOfStateInfo state <> "\""
    , "static istring_t INLINE"
        <+> pretty (nameOfReadFact  state) <+> "("
         <> pretty (nameOfStateType state) <+> "*program,"
        <+> "const char *value_ptr, const size_t value_size, idate_t date)"
    , "{"
    , "    char *p  = (char *) value_ptr;"
    , "    char *pe = (char *) value_ptr + value_size;"
    , ""
    , indent 4 readInput
    , ""
    , "    program->" <> pretty (inputSumBool  input) <> "[program->new_count] = itrue;"
    , "    program->" <> pretty (inputSumError input) <> "[program->new_count] = ierror_tombstone;"
    , "    program->" <> pretty (inputDate     input) <> "[program->new_count] = date;"
    , "    program->new_count++;"
    , ""
    , "    if (program->new_count == psv_max_row_count) {"
    , "        " <> pretty (nameOfProgram state) <> " (program);"
    , "        program->new_count = 0;"
    , "    } else if (program->new_count > psv_max_row_count) {"
    , "        return \"" <> pretty (nameOfReadFact state) <> ": new_count > max_count\";"
    , "    }"
    , ""
    , "    return 0; /* no error */"
    , "}"
    , ""
    ]

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
 = case inputType input of
    StructT st -> seaOfReadStruct st (inputVars input)
    IntT       -> seaOfReadInt       (inputVars input)
    t          -> Left (SeaUnsupportedInputType t)

------------------------------------------------------------------------

seaOfReadInt :: [(Text, ValType)] -> Either SeaError Doc
seaOfReadInt vars
  = case vars of
     [(nx, IntT)]
      -> pure $ vsep
         [ "char *end_ptr;"
         , "program->" <> pretty nx <> "[program->new_count] = strtol (p, &end_ptr, 10);"
         , ""
         , "if (pe != end_ptr)"
         , "  return psv_alloc_error (\"not an integer\", p, pe - p);"
         ]
     _
      -> Left (SeaInputTypeMismatch IntT vars)

------------------------------------------------------------------------

data FieldMapping = FieldMapping {
    _fieldName :: Text
  , _fieldType :: ValType
  , _fieldVars :: [Text]
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
       | t == t'   = Just n
       | otherwise = Nothing

  ns <- zipWithM go (meltType ftype) vars0

  let mapping = FieldMapping fname ftype ns
      vars1   = drop (length ns) vars0

  return (mapping, vars1)

seaOfReadStruct :: StructType -> [(Text, ValType)] -> Either SeaError Doc
seaOfReadStruct st@(StructType fields) vars = do
  let mismatch = SeaStructFieldsMismatch st vars
  mappings     <- maybe (Left mismatch) Right (mappingOfFields (Map.toList fields) vars)
  mappings_sea <- traverse seaOfFieldMapping mappings
  pure $ vsep
    [ "if (*p++ != '{')"
    , "    return psv_alloc_error (\"missing {\",  value_ptr, value_size);"
    , ""
    , "for (ibool_t done = ifalse; !done;) {"
    , "    if (*p++ != '\"')"
    , "        return psv_alloc_error (\"missing \\\"\", value_ptr, value_size);"
    , ""
    , indent 4 (vsep mappings_sea)
    , "    return psv_alloc_error (\"invalid field start\", p, pe - p);"
    , "}"
    ]

seaOfFieldMapping :: FieldMapping -> Either SeaError Doc
seaOfFieldMapping (FieldMapping fname ftype vars) = do
  -- TODO Haskell escapes /= C escapes
  let needle = text (show (fname <> "\""))
  field_sea <- seaOfReadJson ftype vars
  pure $ vsep
    [ "if (memcmp (" <> needle <> ", p, sizeof (" <> needle <> ") - 1) == 0) {"
    , "    p += sizeof (" <> needle <> ") - 1;"
    , indent 4 field_sea
    , "    if (error) return error;"
    , "    continue;"
    , "}"
    , ""
    ]

seaOfReadJson :: ValType -> [Text] -> Either SeaError Doc
seaOfReadJson ftype vars
 = let readJson t n = "psv_error_t error = psv_read_json_" <> t
                   <> " (&p, pe, &program->" <> pretty n <> "[program->new_count], &done);"
   in case (ftype, vars) of
       (OptionT t, [nb, nx]) -> do
         val_sea <- seaOfReadJson t [nx]
         pure $ vsep ["program->" <> pretty nb <+> "= itrue;", val_sea]

       (IntT, [nx]) -> do
         pure (readJson "int" nx)

       (StringT, [nx]) -> do
         pure (readJson "string" nx)

       (DateTimeT, [nx]) -> do
         pure (readJson "date" nx)

       _
        -> Left (SeaUnsupportedStructFieldType ftype vars)

------------------------------------------------------------------------

seaOfWriteFleetOutput :: [SeaProgramState] -> Either SeaError Doc
seaOfWriteFleetOutput states = do
  write_sea <- traverse seaOfWriteProgramOutput states
  pure $ vsep
    [ "#line 1 \"write all outputs\""
    , "static void psv_write_outputs (int fd, const char *entity, ifleet_t *fleet)"
    , "{"
    , "    idate_t snapshot_date = fleet->snapshot_date;"
    , "    iint_t year, month, day, hour, minute, second;"
    , "    idate_to_gregorian (snapshot_date, &year, &month, &day, &hour, &minute, &second);"
    , indent 4 (vsep write_sea)
    , "}"
    ]

seaOfWriteProgramOutput :: SeaProgramState -> Either SeaError Doc
seaOfWriteProgramOutput state = do
  let ps = "p" <> int (stateName state)

  let resumeables = fmap (\(n,_) -> ps <> "->" <> pretty (hasPrefix <> n) <+> "= ifalse;") (stateResumables state)
  outputs <- traverse (\(n,(t,ts)) -> seaOfOutput ps n t ts) (stateOutputs state)

  pure $ vsep
    [ ""
    , "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
    , pretty (nameOfStateType state) <+> "*" <> ps <+> "=" <+> "&fleet->" <> pretty (nameOfProgram state) <> ";"
    , pretty (nameOfProgram state) <+> "(" <> ps <> ");"
    , ps <> "->new_count = 0;"
    , vsep resumeables
    , ""
    , vsep outputs
    ]

seaOfOutput :: Doc -> OutputName -> ValType -> [ValType] -> Either SeaError Doc
seaOfOutput ps oname@(OutputName name) otype ts
  = let members     = List.take (length ts) (fmap (\ix -> ps <> "->" <> seaOfNameIx name ix) [0..])
        attrib      = text (fromMaybe "" (init (drop 1 (show name)))) -- TODO Haskell escapes /= C escapes
        mismatch    = Left (SeaOutputTypeMismatch oname otype ts)
        unsupported = Left (SeaUnsupportedOutputType otype)

        dprintf fmt n = "dprintf (fd, \"%s|" <> attrib <> "|" <> fmt <> "|%lld-%02lld-%02lldT%02lld:%02lld:%02lld\\n\""
                     <> ", entity, " <> n <> ", year, month, day, hour, minute, second);"
    in case otype of
      SumT ErrorT IntT
       | [BoolT, ErrorT, IntT] <- ts
       , [_, _, mx]            <- members
       -> pure (dprintf "%lld" mx)

       | otherwise
       -> mismatch

      SumT ErrorT DoubleT
       | [BoolT, ErrorT, DoubleT] <- ts
       , [_, _, mx]               <- members
       -> pure (dprintf "%f" mx)

       | otherwise
       -> mismatch

      SumT ErrorT StringT
       | [BoolT, ErrorT, StringT] <- ts
       , [_, _, mx]               <- members
       -> pure (dprintf "%s" mx)

       | otherwise
       -> mismatch

      _
       -> unsupported

------------------------------------------------------------------------

-- TODO Haskell escapes /= C escapes
seaOfAttribute :: SeaProgramState -> Doc
seaOfAttribute = text . show . getAttribute . stateAttribute

last :: [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x:) <$> init xs
