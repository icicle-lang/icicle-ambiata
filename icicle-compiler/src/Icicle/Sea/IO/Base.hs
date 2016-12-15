{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO.Base
  ( -- * Mode
    Mode(..)

    -- * State
  , seaOfConfigureFleet
  , seaOfFleetState
  , seaOfAllocFleet
  , seaOfAllocProgram
  , seaOfCollectFleet
  , defOfProgramState
  , defOfState
  , defOfLastTime
  , defOfCount
  , seaOfAssignTime
  , seaOfChordScan
  , seaOfChordTimes

    -- * Input
  , InputOpts (..)
  , InputAllowDupTime (..)
  , CStmt
  , CBlock
  , CName
  , CFun
  , SeaInput (..)
  , SeaInputError (..)
  , seaOfReadNamedFact
  , initType
  , Assignment
  , assignVar
  , assignArrayMutable
  , seaOfArrayPutMutable
  , seaOfStringEq
  , seaOfBytesEq
  , CheckedInput (..)
  , checkInputType
  , FieldMapping (..)
  , mappingOfFields
  , seaOfAssignInput'
  , seaOfAssignInput
  , seaOfDefineInput

    -- * Helpers
  , last
  , init
  , sizeOfString
  , wrapInBlock
  , unArray
  , StringWord(..)
  , wordsOfString
  , wordsOfBytes
  , wordsOfBytes'
  ) where

import qualified Data.ByteString                  as B
import           Data.Map                         (Map)
import           Data.Set                         (Set)
import qualified Data.Text.Encoding               as T
import           Data.Word                        (Word8)
import qualified Data.List as List

import           Text.Printf (printf)

import           Icicle.Avalanche.Prim.Flat       (Prim(..), PrimArray(..))
import           Icicle.Avalanche.Prim.Flat       (meltType)

import           Icicle.Common.Type               (ValType(..), StructField(..))
import           Icicle.Common.Type               (defaultOfType)

import           Icicle.Data                      (Attribute(..), Time)

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Program (seaOfXValue)
import           Icicle.Sea.FromAvalanche.Base (seaOfAttributeDesc, seaOfTime)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P


data Mode
  = Snapshot Time
  | Chords
  deriving (Eq, Ord, Show)

-- State
--------------------------------------------------------------------------------

seaOfConfigureFleet :: Mode -> [SeaProgramState] -> Doc
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
     Snapshot time -> indent 4 (seaOfChordTimes [time])
     Chords        -> indent 4 seaOfChordScan
 , ""
 , "    if (chord_count > max_chord_count) {"
 , "        return ierror_loc_format"
 , "            ( 0, 0"
 , "            , \"exceeded maximum number of chords per entity (chord_count = %\" PRId64 \", max_chord_count = %\" PRId64 \")\""
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
 , indent 4 (vsep (fmap defOfCount states))
 , ""
 , "    return 0;"
 , "}"
 ]

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
      , indent 4 (vsep (fmap defOfProgramCount states))
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

defOfProgramCount :: SeaProgramState -> Doc
defOfProgramCount state
  = defOfVar 0 IntT (pretty (nameOfCount state)) <> ";"
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

       go (n, t) = program
                <> stateInputNew (pretty n)
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
       pvari = pvar <> "input."

       new n = pvari <> pretty (newPrefix <> n)
       res n = pvar  <> pretty (resPrefix <> n)

       copyInputs nts
        = let docs = concatMap copyInput (stateInputVars state)
          in if List.null docs
             then []
             else [ "iint_t new_count = " <> pvari <> "new_count;"
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
  FactIdentifierT -> False

  -- these should have been melted
  PairT{}   -> False
  OptionT{} -> False
  StructT{} -> False
  SumT{}    -> False
  MapT{}    -> False

defOfState :: SeaProgramState -> Doc
defOfState state
 = let stype  = pretty (nameOfStateType state)
       var    = "*p" <> pretty (stateName state)
       member = "fleet->" <> pretty (nameOfProgram state)
   in stype <+> var <+> "=" <+> member <> ";"

defOfLastTime :: SeaProgramState -> Doc
defOfLastTime state
 = "fleet->" <> pretty (nameOfLastTime state) <+> "= 0;"

defOfCount :: SeaProgramState -> Doc
defOfCount state
 = "fleet->" <> pretty (nameOfCount state) <+> "= 0;"

seaOfAssignTime :: SeaProgramState -> Doc
seaOfAssignTime state
 = let ptime = "p" <> pretty (stateName state) <> "[ix].input." <> pretty (stateTimeVar state)
   in  ptime <+> "=" <+> "chord_time;"

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

-- Input
--------------------------------------------------------------------------------

type Name = Text

data InputOpts = InputOpts
  { inputAllowDupTime :: InputAllowDupTime
  , inputTombstones   :: Map Attribute (Set Text)
  } deriving (Show, Eq)

-- | Whether fact times must be unique for an entity.
--
data InputAllowDupTime
  = AllowDupTime | DoNotAllowDupTime
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data CheckedInput = CheckedInput {
    inputSumError :: Name
  , inputTime     :: Name
  , inputType     :: ValType
  , inputVars     :: [(Name, ValType)]
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

--------------------------------------------------------------------------------

type CStmt  = Doc -- lies
type CBlock = Doc
type CFun   = Doc
type CName  = Doc

-- Common input errors, for both PSV and Zebra
data SeaInputError = SeaInputError
  { seaInputErrorTimeOutOfOrder   :: CStmt
    -- ^ what to do when time is out of order
  , seaInputErrorCountExceedLimit :: CStmt
    -- ^ what to do when ent-attr count exceeds limit
  }

-- Common input statements, for both PSV and Zebra
data SeaInput = SeaInput
  { cstmtReadFact     :: SeaProgramState -> Set Text -> CheckedInput -> CStmt -> CStmt -> CFun
  -- ^ Generate C code to read input into the `program->input` struct.
  , cstmtReadTime     :: CStmt
  -- ^ Generate C code to read the current fact time.
  , cfunReadTombstone :: CheckedInput -> [Text] -> CStmt
  -- ^ Generate C code to read the tombstone of this input.
  , cnameFunReadFact  :: SeaProgramState -> CName
  -- ^ Name of the read_fact function. e.g. psv_read_fact_0.
  }

seaOfReadNamedFact :: SeaInput
                   -> SeaInputError
                   -> InputAllowDupTime
                   -> SeaProgramState
                   -> CStmt
seaOfReadNamedFact funs errs allowDupTime state
 = let fun    = cnameFunReadFact funs  state
       pname  = pretty (nameOfProgram  state)
       tname  = pretty (nameOfLastTime state)
       cname  = pretty (nameOfCount    state)
       tcond  = if allowDupTime == AllowDupTime
                then "if (time < last_time)"
                else "if (time <= last_time)"
   in vsep
      [ "{"
      , "    itime_t time;"
      , indent 4 $ cstmtReadTime funs
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
      , indent 4 tcond
      , "    {"
      , "        char curr_time_ptr[text_itime_max_size];"
      , "        size_t curr_time_size = text_write_itime (time, curr_time_ptr);"
      , ""
      , "        char last_time_ptr[text_itime_max_size];"
      , "        size_t last_time_size = text_write_itime (last_time, last_time_ptr);"
      , ""
      , indent 8 $ seaInputErrorTimeOutOfOrder errs
      , "    }"
      , ""
      , "    fleet->" <> tname <> " = time;"
      , ""
      , "    fleet->" <> cname <> " ++;"
      , ""
      , "    if (fleet->" <> cname <> " > facts_limit)"
      , "    {"
      , indent 8 $ seaInputErrorCountExceedLimit errs
      , "    }"
      , ""
      , "    return " <> fun <> " (value_ptr, value_size, time, fleet->mempool, chord_count, fleet->" <> pname <> ");"
      , ""
      , "}"
      , ""
      ]

--------------------------------------------------------------------------------

seaOfAssignInput' :: Text -> Doc
seaOfAssignInput' n
 = "program->input." <> pretty n <> "[new_count] = " <> pretty n <> ";"

seaOfAssignInput :: (Text, ValType) -> Doc
seaOfAssignInput (n, _)
 = seaOfAssignInput' n

seaOfDefineInput :: (Text, ValType) -> Doc
seaOfDefineInput (n, t)
 = seaOfValType t <+> pretty n <> initType t

initType :: ValType -> Doc
initType vt = " = " <> seaOfXValue (defaultOfType vt) vt <> ";"

-- | Describes how to assign to a C struct member, this changes for arrays
type Assignment = Doc -> ValType -> Doc -> Doc

assignVar :: Assignment
assignVar n _ x = pretty n <+> "=" <+> x <> ";"

assignArrayMutable :: Assignment
assignArrayMutable n t x = n <+> "=" <+> seaOfArrayPutMutable n "ix" x t <> ";"

seaOfArrayPutMutable :: Doc -> Doc -> Doc -> ValType -> Doc
seaOfArrayPutMutable arr ix val typ
 = seaOfPrimDocApps (seaOfXPrim (PrimArray (PrimArrayPutMutable typ)))
                    [ arr, ix, val ]

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

wrapInBlock :: Doc -> Doc
wrapInBlock x
  = vsep ["{", indent 4 x, "}"]

sizeOfString :: Text -> Int
sizeOfString = B.length . T.encodeUtf8

unArray :: (Text, ValType) -> Either SeaError (Text, ValType)
unArray (n, ArrayT t) = Right (n, t)
unArray (n, t)        = Left (SeaInputTypeMismatch t [(n, t)])

last :: [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x:) <$> init xs

--------------------------------------------------------------------------------

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
