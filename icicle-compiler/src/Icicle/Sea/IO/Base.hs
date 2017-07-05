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

import           Icicle.Data.Name
import           Icicle.Data.Time (Time)

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Program (seaOfXValue)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type
import           Icicle.Sea.Name

import           P


data Mode
  = Snapshot Time
  | Chords
  deriving (Eq, Ord, Show)

-- State
--------------------------------------------------------------------------------

seaOfConfigureFleet :: Mode -> [Cluster c k] -> Doc
seaOfConfigureFleet mode clusters
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
 , indent 4 (vsep (fmap defOfState clusters))
 , ""
 , "    for (iint_t ix = 0; ix < chord_count; ix++) {"
 , "        itime_t  chord_time = fleet->chord_times[ix];"
 , indent 8 (vsep (fmap seaOfAssignTime clusters))
 , "    }"
 , ""
 , indent 4 (vsep (fmap defOfLastTime clusters))
 , ""
 , indent 4 (vsep (fmap defOfCount clusters))
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


seaOfFleetState :: [Cluster c k] -> Doc
seaOfFleetState clusters
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
      , indent 4 (vsep (fmap defOfProgramState clusters))
      , indent 4 (vsep (fmap defOfProgramTime  clusters))
      , indent 4 (vsep (fmap defOfProgramCount clusters))
      , "};"
      ]

defOfProgramState :: Cluster c k -> Doc
defOfProgramState cluster
 = defOfVar' 1 (pretty (nameOfClusterState cluster))
               (pretty (nameOfCluster cluster)) <> ";"
 <+> "/* " <> (prettyText . renderInputId $ clusterInputId cluster) <> " */"

defOfProgramTime :: Cluster c k -> Doc
defOfProgramTime cluster
 = defOfVar 0 TimeT (pretty (nameOfLastTime cluster)) <> ";"
 <+> "/* " <> (prettyText . renderInputId $ clusterInputId cluster) <> " */"

defOfProgramCount :: Cluster c k -> Doc
defOfProgramCount cluster
  = defOfVar 0 IntT (pretty (nameOfCount cluster)) <> ";"
  <+> "/* " <> (prettyText . renderInputId $ clusterInputId cluster) <> " */"

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

seaOfAllocFleet :: [Cluster c k] -> Doc
seaOfAllocFleet clusters
 = vsep
 [ "#line 1 \"allocate fleet state\""
 , "static ifleet_t * psv_alloc_fleet (iint_t max_chord_count, iint_t max_row_count, iint_t max_map_size)"
 , "{"
 , "    ifleet_t *fleet = calloc (1, sizeof (ifleet_t));"
 , ""
 , "    fleet->max_chord_count    = max_chord_count;"
 , "    fleet->chord_times        = calloc (max_chord_count, sizeof(itime_t));"
 , ""
 , indent 4 (vsep (fmap seaOfAllocProgram clusters))
 , "    return fleet;"
 , "}"
 ]

seaOfAllocProgram :: Cluster c k -> Doc
seaOfAllocProgram cluster
 = let programs  = "fleet->" <> pretty (nameOfCluster cluster)
       program   = programs <> "[ix]."
       stype     = pretty (nameOfClusterState cluster)

       calloc n t = "calloc (" <> n <> ", sizeof (" <> t <> "));"

       go (n, t) = program
                <> clusterInputNew (pretty n)
                <> " = "
                <> calloc "max_row_count" (seaOfValType t)

       inputVars = fmap (first renderSeaName) $ clusterInputVars cluster

   in vsep [ "/* " <> (prettyText . renderInputId $ clusterInputId cluster) <> " */"
           , programs <> " = " <> calloc "max_chord_count" stype
           , ""
           , "for (iint_t ix = 0; ix < max_chord_count; ix++) {"
           , indent 4 (program <> "max_map_size = max_map_size;")
           , indent 4 (vsep (fmap go inputVars))
           , "}"
           , ""
           ]

------------------------------------------------------------------------

seaOfCollectFleet :: [Cluster c k] -> Doc
seaOfCollectFleet clusters
 = vsep
 [ "#line 1 \"collect fleet state\""
 , "static void psv_collect_fleet (ifleet_t *fleet)"
 , "{"
 , "    anemone_mempool_t *into_pool        = anemone_mempool_create ();"
 , "    anemone_mempool_t *last_pool        = fleet->mempool;"
 , "    iint_t             max_chord_count  = fleet->max_chord_count;"
 , "    iint_t             chord_count      = fleet->chord_count;"
 , ""
 , indent 4 (vsep (fmap seaOfCollectProgram clusters))
 , ""
 , "    fleet->mempool = into_pool;"
 , ""
 , "    for (iint_t ix = 0; ix < max_chord_count; ix++) {"
 , indent 8 (vsep (fmap seaOfAssignMempool clusters))
 , "    }"
 , ""
 , "    if (last_pool != 0) {"
 , "        anemone_mempool_free (last_pool);"
 , "    }"
 , "}"
 ]

seaOfAssignMempool :: Cluster c k -> Doc
seaOfAssignMempool cluster
 = let pname = pretty (nameOfCluster cluster)
   in "fleet->" <> pname <> "[ix].mempool = into_pool;"

seaOfCollectProgram :: Cluster c k -> Doc
seaOfCollectProgram cluster
 = let pname = pretty (nameOfCluster cluster)
       stype = pretty (nameOfClusterState cluster)
       pvar  = "program->"
       pvari = pvar <> "input."

       new n = pvari <> pretty (newPrefix <> n)
       res n = pvar  <> pretty resPrefix <> n

       inputVars = fmap (first renderSeaName) $ clusterInputVars cluster
       resVarsOf k = fmap (first renderSeaName) $ kernelResumables k

       copyInputs nts
        = let docs = concatMap copyInput inputVars
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

   in vsep [ "/* " <> (prettyText . renderInputId $ clusterInputId cluster) <> " */"
           , "for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
           , indent 4 $ stype <+> "*program = &fleet->" <> pname <> "[chord_ix];"
           , ""
           , "    if (last_pool != 0) {"
           , indent 8 $ vsep $ copyInputs inputVars
                            <> concatMap
                                 (\c -> concatMap (copyResumable c) (resVarsOf c))
                                 (NonEmpty.toList (clusterKernels cluster))
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

defOfState :: Cluster c k -> Doc
defOfState cluster
 = let stype  = pretty (nameOfClusterState cluster)
       var    = "*p" <> prettyClusterId (clusterId cluster)
       member = "fleet->" <> pretty (nameOfCluster cluster)
   in stype <+> var <+> "=" <+> member <> ";"

defOfLastTime :: Cluster c k -> Doc
defOfLastTime cluster
 = "fleet->" <> pretty (nameOfLastTime cluster) <+> "= 0;"

defOfCount :: Cluster c k -> Doc
defOfCount cluster
 = "fleet->" <> pretty (nameOfCount cluster) <+> "= 0;"

seaOfAssignTime :: Cluster c k -> Doc
seaOfAssignTime cluster =
  let
    ptime =
      "p" <> prettyClusterId (clusterId cluster) <>
      "[ix].input." <> prettySeaName (clusterTimeVar cluster)
  in
    ptime <+> "=" <+> "chord_time;"

-- Input
--------------------------------------------------------------------------------

type Name = Text

data InputOpts = InputOpts
  { inputAllowDupTime :: InputAllowDupTime
  , inputTombstones   :: Map InputId (Set Text)
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

checkInputType :: Cluster c k -> Either SeaError CheckedInput
checkInputType cluster
 = case clusterInputType cluster of
     PairT (SumT ErrorT t) TimeT
      | (sumError, ErrorT) : xs0 <- clusterInputVars cluster
      , Just vars                <- init xs0
      , Just (time, TimeT)       <- last xs0
      -> Right CheckedInput {
             inputSumError = newPrefix <> renderSeaName sumError
           , inputTime     = newPrefix <> renderSeaName time
           , inputType     = t
           , inputVars     = fmap (first ((newPrefix <>) . renderSeaName)) vars
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
