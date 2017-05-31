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
  , seaOfSnapshotTime

    -- * Input
  , InputOpts (..)
  , InputAllowDupTime (..)
  , CStmt
  , CBlock
  , CName
  , CFun
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
import qualified Data.List.NonEmpty as NonEmpty


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

seaOfConfigureFleet :: Mode -> [SeaProgramAttribute] -> Doc
seaOfConfigureFleet mode states
 = vsep
 [ "#line 1 \"configure fleet state\""
 , "static ierror_loc_t psv_configure_fleet "
 , "    (const char *entity, size_t entity_size, piano_t *piano, ifleet_t *fleet)"
 , "{"
 , "    iint_t max_chord_count = fleet->max_chord_count;"
 , ""
 , "    iint_t   chord_count = 0;"
 , "    itime_t *chord_times = NULL;"
 , "    int64_t *chord_name_offsets = NULL;"
 , "    int64_t *chord_name_lengths = NULL;"
 , "    uint8_t *chord_name_data = NULL;"
 , ""
 , indent 4 lookup
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
 , ""
 , indent 4 conv
 , indent 4 (vsep (fmap defOfState states))
 , ""
 , "    for (iint_t ix = 0; ix < chord_count; ix++) {"
 , "        itime_t  chord_time = fleet->chord_times[ix];"
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
 where
   (lookup, conv)
     = case mode of
         Snapshot time
           -> (seaOfSnapshotTime time, use_times)
         Chords
           -> (seaOfPianoLookup, conv_times)

   use_times
     = "fleet->chord_times = chord_times;"

   conv_times
     = vsep
     [ "fleet->chord_name_data = chord_name_data;"
     , "fleet->chord_name_offsets = chord_name_offsets;"
     , "fleet->chord_name_lengths = chord_name_lengths;"
     , ""
     , "for (iint_t ix = 0; ix < chord_count; ix++) {"
     , "    fleet->chord_times[ix] = itime_from_epoch_seconds(chord_times[ix]);"
     , "}"
     , ""
     ]


seaOfFleetState :: [SeaProgramAttribute] -> Doc
seaOfFleetState states
 = let time = seaOfValType TimeT
   in vsep
      [ "#line 1 \"fleet state\""
      , "struct ifleet {"
      , indent 4 (defOfVar' 1 "anemone_mempool_t" "mempool")   <> ";"
      , indent 4 (defOfVar  0 IntT      "max_chord_count")     <> ";"
      , indent 4 (defOfVar  0 IntT      "chord_count")         <> ";"
      , indent 4 (defOfVar' 1 time      "chord_times")         <> ";"
      , indent 4 (defOfVar  1 IntT      "chord_name_offsets")  <> ";"
      , indent 4 (defOfVar  1 IntT      "chord_name_lengths")  <> ";"
      , indent 4 (defOfVar' 1 "uint8_t" "chord_name_data")     <> ";"
      , indent 4 (vsep (fmap defOfProgramState states))
      , indent 4 (vsep (fmap defOfProgramTime  states))
      , indent 4 (vsep (fmap defOfProgramCount states))
      , "};"
      ]

defOfProgramState :: SeaProgramAttribute -> Doc
defOfProgramState state
 = defOfVar' 1 (pretty (nameOfStateType state))
               (pretty (nameOfAttribute state)) <> ";"
 <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

defOfProgramTime :: SeaProgramAttribute -> Doc
defOfProgramTime state
 = defOfVar 0 TimeT (pretty (nameOfLastTime state)) <> ";"
 <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

defOfProgramCount :: SeaProgramAttribute -> Doc
defOfProgramCount state
  = defOfVar 0 IntT (pretty (nameOfCount state)) <> ";"
  <+> "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"

seaOfSnapshotTime :: Time -> Doc
seaOfSnapshotTime time
 = vsep
 [ "static itime_t entity_times[] = { " <> seaOfTime time <> " };"
 , ""
 , "chord_count = 1;"
 , "chord_times = entity_times;"
 ]

seaOfPianoLookup :: Doc
seaOfPianoLookup
 = vsep
 [ "piano_lookup"
 , "  ( piano"
 , "  , (const uint8_t*)entity"
 , "  , entity_size"
 , "  , &chord_count"
 , "  , (const int64_t**)&chord_times"
 , "  , (const int64_t**)&chord_name_offsets"
 , "  , (const int64_t**)&chord_name_lengths"
 , "  , (const uint8_t**)&chord_name_data"
 , "  );"
 ]

------------------------------------------------------------------------

seaOfAllocFleet :: [SeaProgramAttribute] -> Doc
seaOfAllocFleet states
 = vsep
 [ "#line 1 \"allocate fleet state\""
 , "static ifleet_t * psv_alloc_fleet (iint_t max_chord_count, iint_t max_row_count, iint_t max_map_size)"
 , "{"
 , "    ifleet_t *fleet = calloc (1, sizeof (ifleet_t));"
 , ""
 , "    fleet->max_chord_count    = max_chord_count;"
 , "    fleet->chord_times        = calloc (max_chord_count, sizeof(itime_t));"
 , ""
 , indent 4 (vsep (fmap seaOfAllocProgram states))
 , "    return fleet;"
 , "}"
 ]

seaOfAllocProgram :: SeaProgramAttribute -> Doc
seaOfAllocProgram state
 = let programs  = "fleet->" <> pretty (nameOfAttribute state)
       program   = programs <> "[ix]."
       stype     = pretty (nameOfStateType state)

       calloc n t = "calloc (" <> n <> ", sizeof (" <> t <> "));"

       go (n, t) = program
                <> stateInputNew (pretty n)
                <> " = "
                <> calloc "max_row_count" (seaOfValType t)

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , programs <> " = " <> calloc "max_chord_count" stype
           , ""
           , "for (iint_t ix = 0; ix < max_chord_count; ix++) {"
           , indent 4 (program <> "max_map_size = max_map_size;")
           , indent 4 (vsep (fmap go (stateInputVars state)))
           , "}"
           , ""
           ]

------------------------------------------------------------------------

seaOfCollectFleet :: [SeaProgramAttribute] -> Doc
seaOfCollectFleet states
 = vsep
 [ "#line 1 \"collect fleet state\""
 , "static void psv_collect_fleet (ifleet_t *fleet)"
 , "{"
 , "    anemone_mempool_t *into_pool        = anemone_mempool_create ();"
 , "    anemone_mempool_t *last_pool        = fleet->mempool;"
 , "    iint_t             max_chord_count  = fleet->max_chord_count;"
 , "    iint_t             chord_count      = fleet->chord_count;"
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
 , "        anemone_mempool_free (last_pool);"
 , "    }"
 , "}"
 ]

seaOfAssignMempool :: SeaProgramAttribute -> Doc
seaOfAssignMempool state
 = let pname = pretty (nameOfAttribute state)
   in "fleet->" <> pname <> "[ix].mempool = into_pool;"

seaOfCollectProgram :: SeaProgramAttribute -> Doc
seaOfCollectProgram state
 = let pname = pretty (nameOfAttribute state)
       stype = pretty (nameOfStateType state)
       pvar  = "program->"
       pvari = pvar <> "input."

       new n = pvari <> pretty (newPrefix <> n)
       res n = pvar  <> pretty resPrefix <> n

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

       copyResumable c (n, t)
        | not (needsCopy t)
        = []

        | otherwise
        = [ ""
          , "if (" <> pvar <> pretty hasPrefix <> nameOfResumable c (pretty n) <> ") {"
          , indent 4 (res (nameOfResumable c $ pretty n) <> " = " <> prefixOfValType t <> "copy (into_pool, " <> res (nameOfResumable c $ pretty n) <> ");")
          , "}"
          ]

   in vsep [ "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
           , "for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
           , indent 4 $ stype <+> "*program = &fleet->" <> pname <> "[chord_ix];"
           , ""
           , "    if (last_pool != 0) {"
           , indent 8 $ vsep $ copyInputs (stateInputVars state)
                            <> concatMap (\c -> concatMap (copyResumable c) $ stateResumables c) (NonEmpty.toList $ stateComputes state)
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

defOfState :: SeaProgramAttribute -> Doc
defOfState state
 = let stype  = pretty (nameOfStateType state)
       var    = "*p" <> pretty (stateAttributeName state)
       member = "fleet->" <> pretty (nameOfAttribute state)
   in stype <+> var <+> "=" <+> member <> ";"

defOfLastTime :: SeaProgramAttribute -> Doc
defOfLastTime state
 = "fleet->" <> pretty (nameOfLastTime state) <+> "= 0;"

defOfCount :: SeaProgramAttribute -> Doc
defOfCount state
 = "fleet->" <> pretty (nameOfCount state) <+> "= 0;"

seaOfAssignTime :: SeaProgramAttribute -> Doc
seaOfAssignTime state
 = let ptime = "p" <> pretty (stateAttributeName state) <> "[ix].input." <> pretty (stateTimeVar state)
   in  ptime <+> "=" <+> "chord_time;"

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

checkInputType :: SeaProgramAttribute -> Either SeaError CheckedInput
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
