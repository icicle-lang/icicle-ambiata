{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.Psv (
    PsvConfig(..)
  , seaOfPsvDriver
  ) where

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Avalanche.Prim.Flat (meltType)

import           Icicle.Common.Base (OutputName(..))
import           Icicle.Common.Type (ValType(..), StructType(..), StructField(..))

import           Icicle.Data (Attribute(..), DateTime)

import           Icicle.Internal.Pretty
import qualified Icicle.Internal.Pretty as Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P


------------------------------------------------------------------------

data PsvConfig = PsvConfig {
    psvSnapshotDate :: DateTime
  , psvTombstones   :: Map Attribute (Set Text)
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

seaOfPsvDriver :: [SeaProgramState] -> PsvConfig -> Either SeaError Doc
seaOfPsvDriver states config = do
  let struct_sea    = seaOfFleetState   states
      alloc_sea     = seaOfAllocFleet   states config
      collect_sea   = seaOfCollectFleet states
  read_sea  <- seaOfReadAnyFact      states config
  write_sea <- seaOfWriteFleetOutput states
  pure $ vsep
    [ struct_sea
    , ""
    , alloc_sea
    , ""
    , collect_sea
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

seaOfAllocFleet :: [SeaProgramState] -> PsvConfig -> Doc
seaOfAllocFleet states config
 = vsep
 [ "#line 1 \"allocate fleet state\""
 , "static ifleet_t * psv_alloc_fleet ()"
 , "{"
 , "    idate_t date = " <> seaOfDate (psvSnapshotDate config) <> ";"
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
           , ps <> pretty (stateDateVar state) <> " = date;"
           , vsep (fmap go (stateInputVars state))
           , ""
           ]

------------------------------------------------------------------------

seaOfCollectFleet :: [SeaProgramState] -> Doc
seaOfCollectFleet states
 = vsep
 [ "#line 1 \"collect fleet state\""
 , "static void psv_collect_fleet (ifleet_t *fleet)"
 , "{"
 , "    imempool_t *into_pool = imempool_create ();"
 , "    imempool_t *last_pool = 0;"
 , ""
 , indent 4 (vsep (fmap seaOfCollectProgram states))
 , ""
 , "    if (last_pool != 0) {"
 , "        imempool_free(last_pool);"
 , "    }"
 , "}"
 ]

seaOfCollectProgram :: SeaProgramState -> Doc
seaOfCollectProgram state
 = let ps        = "fleet->" <> pretty (nameOfProgram state) <> "."
       res n     = ps <> pretty (resPrefix <> n)
       go (n, t) = "if (" <> ps <> pretty (hasPrefix <> n) <> ")"
                <> line
                <> indent 4 (res n <> " = " <> prefixOfValType t <> "copy (into_pool, " <> res n <> ");")

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , "last_pool = " <> ps <> "mempool;"
           , "if (last_pool != 0) {"
           , indent 4 $ vsep $ fmap go $ stateResumables state
           , "}"
           , ps <> "mempool = into_pool;"
           , ""
           ]


------------------------------------------------------------------------

seaOfReadAnyFact :: [SeaProgramState] -> PsvConfig -> Either SeaError Doc
seaOfReadAnyFact states config = do
  let tss = fmap (lookupTombstones config) states
  readStates_sea <- zipWithM seaOfReadFact states tss
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
 , "if (" <> seaOfStringEq (getAttribute (stateAttribute state)) "attrib" "attrib_size" <> ") {"
 , "    return " <> pretty (nameOfReadFact state)
                 <> " (&fleet->" <> pretty (nameOfProgram state) <> ", value, value_size, date);"
 , "}"
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
    , "static istring_t INLINE"
        <+> pretty (nameOfReadFact  state) <+> "("
         <> pretty (nameOfStateType state) <+> "*program,"
        <+> "const char *value_ptr, const size_t value_size, idate_t date)"
    , "{"
    , "    " <> align (seaOfReadTombstone input (Set.toList tombstones)) <> "{"
    , "        program->" <> pretty (inputSumBool input) <> "[program->new_count] = itrue;"
    , indent 8 readInput
    , "    }"
    , ""
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

seaOfReadTombstone :: CheckedInput -> [Text] -> Doc
seaOfReadTombstone input = \case
  []     -> Pretty.empty
  (t:ts) -> "if (" <> seaOfStringEq t "value_ptr" "value_size" <> ") {" <> line
         <> "    program->" <> pretty (inputSumBool input)
                            <> "[program->new_count] = ifalse;" <> line
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
        [ ""
        , "if (" <> seaOfStringEq "true" "value_ptr" "value_size" <> ") {"
        , "    program->" <> pretty nx <> "[program->new_count] = itrue;"
        , "} else if (" <> seaOfStringEq "false" "value_ptr" "value_size" <> ") {"
        , "    program->" <> pretty nx <> "[program->new_count] = ifalse;"
        , "} else {"
        , "    return psv_alloc_error (\"not a boolean\", value_ptr, value_size);"
        , "}"
        ]

    ([(nx, DoubleT)], DoubleT)
     -> pure $ vsep
        [ "char *end_ptr;"
        , "program->" <> pretty nx <> "[program->new_count] = strtod (value_ptr, &end_ptr);"
        , ""
        , "if (value_ptr + value_size != end_ptr)"
        , "  return psv_alloc_error (\"not an number\", value_ptr, value_size);"
        ]

    ([(nx, IntT)], IntT)
     -> pure $ vsep
        [ "char *end_ptr;"
        , "program->" <> pretty nx <> "[program->new_count] = strtol (value_ptr, &end_ptr, 10);"
        , ""
        , "if (value_ptr + value_size != end_ptr)"
        , "  return psv_alloc_error (\"not an integer\", value_ptr, value_size);"
        ]

    ([(nx, DateTimeT)], DateTimeT)
     -> pure $ vsep
        [ "psv_error_t " <> seaOfError nx <> " = "
                         <> "psv_read_date (value_ptr, value_size, "
                         <> "&program->" <> pretty nx <> "[program->new_count]);"
        , "if (" <> seaOfError nx <> ") return " <> seaOfError nx <> ";"
        ]

    ([(nx, StringT)], StringT)
     -> pure $ vsep
        [ "size_t copy_size = value_size + 1;"
        , "char  *copy_ptr  = imempool_alloc (program->mempool, copy_size);"
        , "memcpy (copy_ptr, value_ptr, value_size);"
        , "copy_ptr[copy_size] = 0;"
        , "program->" <> pretty nx <> "[program->new_count] = copy_ptr;"
        ]

    ([(nx, UnitT)], StructT (StructType fs))
     | Map.null fs
     -> pure $ "program->" <> pretty nx <> "[program->new_count] = iunit;"

    (_, StructT st)
     -> seaOfReadStruct st (inputVars input)

    (_, t)
     -> Left (SeaUnsupportedInputType t)

seaOfError :: Text -> Doc
seaOfError suf = "error_" <> pretty suf

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
    [ "char  *p  = (char *) value_ptr;"
    , "char  *pe = (char *) value_ptr + value_size;"
    , ""
    , "if (*p++ != '{')"
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
  let needle = seaOfString (fname <> "\"")
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
 = let readJson  n t  = "psv_error_t error = psv_read_json_" <> t
                     <> " (program->mempool, &p, pe, &program->" <> pretty n <> "[program->new_count], &done);"

       readConst n xx = "program->" <> pretty n <+> "[program->new_count] = " <> xx <> ";"
   in case (ftype, vars) of
       (OptionT t, [nb, nx]) -> do
         val_sea <- seaOfReadJson t [nx]
         pure $ vsep
           [ readConst nb "itrue"
           , val_sea ]

       (BoolT, [nx]) -> do
         pure (readJson nx "bool")

       (IntT, [nx]) -> do
         pure (readJson nx "int")

       (DoubleT, [nx]) -> do
         pure (readJson nx "double")

       (DateTimeT, [nx]) -> do
         pure (readJson nx "date")

       (StringT, [nx]) -> do
         pure (readJson nx "string")

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
    , indent 4 (vsep write_sea)
    , "}"
    ]

seaOfWriteProgramOutput :: SeaProgramState -> Either SeaError Doc
seaOfWriteProgramOutput state = do
  let ps = "p" <> int (stateName state)

  let resumeables = fmap (\(n,_) -> ps <> "->" <> pretty (hasPrefix <> n) <+> "= ifalse;") (stateResumables state)
  outputs <- traverse (\(n,(t,ts)) -> seaOfOutput ps n t ts 0) (stateOutputs state)

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

seaOfOutput :: Doc -> OutputName -> ValType -> [ValType] -> Int -> Either SeaError Doc
seaOfOutput ps oname@(OutputName name) otype0 ts0 ixStart
  = let members     = List.take (length ts0) (fmap (\ix -> ps <> "->" <> seaOfNameIx name ix) [ixStart..])
        attrib      = seaOfEscaped name
        mismatch    = Left (SeaOutputTypeMismatch oname otype0 ts0)
        unsupported = Left (SeaUnsupportedOutputType otype0)

        dateFmt       = "%lld-%02lld-%02lldT%02lld:%02lld:%02lld"
        dprintf fmt n = "dprintf (fd, \"%s|" <> attrib <> "|" <> fmt <> "\\n\"" <> ", entity, " <> n <> ");"
    in case otype0 of
      SumT ErrorT otype1
       | (BoolT : ErrorT : ts1) <- ts0
       , (nb    : _      : _)   <- members
       -> do doc <- seaOfOutput ps oname otype1 ts1 (ixStart+2)
             pure ("if (" <> nb <> ")" <+> doc)

       | otherwise
       -> mismatch

      BoolT
       | [BoolT] <- ts0
       , [mx]   <- members
       -> pure $ vsep
          [ "if (" <> mx <> ") {"
          , indent 4 (dprintf "%s" "\"true\"")
          , "} else {"
          , indent 4 (dprintf "%s" "\"false\"")
          , "}"
          ]

       | otherwise
       -> mismatch

      IntT
       | [IntT] <- ts0
       , [mx]   <- members
       -> pure (dprintf "%lld" mx)

       | otherwise
       -> mismatch

      DoubleT
       | [DoubleT] <- ts0
       , [mx]      <- members
       -> pure (dprintf "%f" mx)

       | otherwise
       -> mismatch

      StringT
       | [StringT] <- ts0
       , [mx]      <- members
       -> pure (dprintf "%s" mx)

       | otherwise
       -> mismatch

      DateTimeT
       | [DateTimeT] <- ts0
       , [mx]        <- members
       -> pure $ vsep
          [ "{"
          , "    iint_t v_year, v_month, v_day, v_hour, v_minute, v_second;"
          , "    idate_to_gregorian (" <> mx <> ", &v_year, &v_month, &v_day, &v_hour, &v_minute, &v_second);"
          , indent 4 (dprintf dateFmt "v_year, v_month, v_day, v_hour, v_minute, v_second")
          , "}"
          ]

       | otherwise
       -> mismatch

      _
       -> unsupported

------------------------------------------------------------------------

seaOfStringEq :: Text -> Doc -> Doc -> Doc
seaOfStringEq str ptr size =
  align $ vsep [ size <+> "==" <+> "sizeof (" <> seaOfString str <> ") - 1 &&"
               , "memcmp (" <> seaOfString str <> ", " <> ptr <> ", " <> size <> ") == 0" ]

lookupTombstones :: PsvConfig -> SeaProgramState -> Set Text
lookupTombstones config state =
  fromMaybe Set.empty (Map.lookup (stateAttribute state) (psvTombstones config))

last :: [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x:) <$> init xs
