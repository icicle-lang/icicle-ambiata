-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE LambdaCase        #-}

module Icicle.Sea.IO.Zebra (
    seaOfZebraDriver

    -- * Input
  , ZebraInputConfig (..)
  , ZebraChunkFactCount (..)
  , ZebraAllocLimitGB (..)
  , defaultZebraInputConfig
  , defaultZebraChunkFactCount
  , defaultZebraAllocLimitGB

    -- * Output
  , ZebraOutputConfig (..)
  , ZebraOutputError (..)
  , zebraEmptyTable
  , schemaOfFleetOutput
  , foreignSchemaOfFleetOutput
  ) where

import           Control.Monad.IO.Class

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import           System.IO (IO, FilePath)

import           Foreign.Storable (pokeElemOff)
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal.Array (mallocArray)

import           P

import qualified X.Data.Vector.Cons as Cons
import           X.Control.Monad.Trans.Either

import           Anemone.Foreign.Mempool (Mempool)

import qualified Zebra.Table.Striped as Striped
import qualified Zebra.Table.Schema as Schema
import qualified Zebra.Table.Data as Table
import qualified Zebra.Table.Encoding as Table

import qualified Zebra.Foreign.Table as Zebra
import qualified Zebra.Foreign.Bindings as Zebra

import qualified Zebra.Serial.Binary.Striped as Zebra

import           Icicle.Common.Type

import           Icicle.Data (Attribute (..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Offset
import           Icicle.Sea.FromAvalanche.State


seaOfZebraDriver :: [Attribute] -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfZebraDriver concretes states = do
  sea_read <- seaOfReadZebraDriver concretes states
  sea_write <- seaOfWriteZebraDriver concretes states
  return . vsep $
    [ seaOfAttributeCount concretes
    , sea_read
    , sea_write
    ]

seaOfAttributeCount :: [Attribute] -> Doc
seaOfAttributeCount all_attributes = vsep
  [ "static int64_t zebra_attribute_count()"
  , "{"
  , "    return " <> pretty (length all_attributes) <> ";"
  , "}"
  , ""
  ]

seaOfReadZebraDriver :: [Attribute] -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfReadZebraDriver concretes states =
  seaOfDefRead <$> indexedPrograms concretes states

seaOfWriteZebraDriver :: [Attribute] -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfWriteZebraDriver concretes states =
  seaOfDefWrite <$> indexedPrograms concretes states

indexedPrograms :: [Attribute] -> [SeaProgramAttribute] -> Either SeaError [(Int, SeaProgramAttribute)]
indexedPrograms concretes states = do
  let lookup x = maybeToRight (SeaNoAttributeIndex x) $ List.elemIndex x concretes
  indices <- sequence $ fmap (lookup . stateAttribute) states
  return $ List.zip indices states

-- * Input

data ZebraInputConfig = ZebraInputConfig {
    zebraChunkFactCount  :: !ZebraChunkFactCount
  , zebraAllocLimitGB :: !ZebraAllocLimitGB
  } deriving (Eq, Ord, Show)

newtype ZebraChunkFactCount = ZebraChunkFactCount { unZebraChunkFactCount :: Int }
  deriving (Eq, Ord, Show)

newtype ZebraAllocLimitGB = ZebraAllocLimitGB { unZebraAllocLimitGB :: Int }
  deriving (Eq, Ord, Show)

defaultZebraInputConfig :: ZebraInputConfig
defaultZebraInputConfig = ZebraInputConfig defaultZebraChunkFactCount defaultZebraAllocLimitGB

defaultZebraChunkFactCount :: ZebraChunkFactCount
defaultZebraChunkFactCount = ZebraChunkFactCount 128

defaultZebraAllocLimitGB :: ZebraAllocLimitGB
defaultZebraAllocLimitGB = ZebraAllocLimitGB 2

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
  , ""
  , "        state->entity_alloc_count = mempool->total_alloc_size;"
  , "    }"
  , ""
  , "    return 0; /* no error */"
  , "}"
  , ""
  ]
 where
  computes = NonEmpty.toList $ stateComputes state

nameOfRead :: SeaProgramAttribute -> CName
nameOfRead state = pretty ("zebra_read_entity_" <> pretty (nameOfAttribute state))

--------------------------------------------------------------------------------

-- * Output

data ZebraOutputConfig = ZebraOutputConfig {
    zebraOutputFilePath :: FilePath
  } deriving (Eq, Show)

data ZebraOutputError =
    ZebraUnexpectedStruct StructType
  | ZebraEncodeError Zebra.BinaryStripedEncodeError
  deriving (Show)

instance Pretty ZebraOutputError where
  pretty = \case
    ZebraUnexpectedStruct st ->
      "Zebra output encounters empty struct type: " <> pretty st
    ZebraEncodeError e ->
      "Zebra encode error: " <> pretty (show e)

-- zebra_output_fleet ( zebra_state_t *stae, zebra_table_t **dst ) {
--   int64_t input_fields_count = ...;
--   /* attribute 0 */
--   zebra_output_table (&(fleet->iprogram_0->mempool) + input_fields_count , dst)
--   ...
-- }
seaOfDefWrite :: [(Int, SeaProgramAttribute)] -> Doc
seaOfDefWrite states = vsep
  [ "#line 1 \"write fleet\""
  , "static ierror_msg_t zebra_output_fleet (zebra_state_t *state, zebra_table_t **dst)"
  , "{"
  , "    ifleet_t    *fleet = state->fleet;"
  , "    int64_t      input_fields_count = 0;"
  , "    int64_t      offset = 0;"
  , ""
  , indent 4 . vsep $ fmap (uncurry seaOfWrite) states
  , "    return 0;"
  , "}"
  , ""
  ]


-- typedef struct {
--     anemone_mempool_t *mempool;
--     input_pixel_article_t input;
--     /* outputs */
--     ierror_t         a_ix_0;
--     ibool_t          a_ix_1;
-- ...
-- } iattribute0_t
--
seaOfWrite :: Int -> SeaProgramAttribute -> Doc
seaOfWrite index state = vsep
  [ "/*" <> n <> " (index " <> pretty index <> "): " <> a <> " */"
  , "input_fields_count = " <> pretty (inputFieldsCount state) <> ";"
  , "offset = zebra_output_table"
  , "            ( &(fleet->" <> n <> "->mempool) + input_fields_count"
  , "            , dst + offset);"
  ]
  where
    n = pretty (nameOfAttribute state)
    a = pretty (getAttribute (stateAttribute state))

foreignSchemaOfFleetOutput ::
     Mempool
  -> [SeaProgramAttribute]
  -> EitherT ZebraOutputError IO (Ptr (Ptr Zebra.C'zebra_table))
foreignSchemaOfFleetOutput pool states = do
  tables <- schemaOfFleetOutput states
  ptrs <- mapM (Zebra.foreignOfTable pool) tables
  array <- liftIO $ mallocArray (length ptrs)
  forM_ (List.zip ptrs [0..]) $ \(ptr, ix) ->
    liftIO $ pokeElemOff array ix (Zebra.unCTable ptr)
  return array

schemaOfFleetOutput :: [SeaProgramAttribute] -> EitherT ZebraOutputError IO [Striped.Table]
schemaOfFleetOutput =
  fmap concat . traverse schemaOfProgramOutput

schemaOfProgramOutput ::
     SeaProgramAttribute
  -> EitherT ZebraOutputError IO [Striped.Table]
schemaOfProgramOutput state = do
  let
    outputs =
      fmap (fst . snd) (stateOutputs . NonEmpty.head . stateComputes $ state)
  hoistEither . sequence . fmap zebraEmptyTable $ outputs

zebraEmptyTable :: ValType -> Either ZebraOutputError Striped.Table
zebraEmptyTable outputType =
  Striped.Array <$> zebraEmptyColumn outputType

zebraEmptyColumn :: ValType -> Either ZebraOutputError Striped.Column
zebraEmptyColumn = fmap Striped.emptyColumn . zebraColumnSchema

zebraColumnSchema :: ValType -> Either ZebraOutputError Schema.Column
zebraColumnSchema outputType =
  case outputType of
    BoolT ->
      pure . Schema.Enum $ Cons.from2 Schema.true Schema.false
    TimeT ->
      pure $ Schema.Int
    DoubleT ->
      pure $ Schema.Double
    IntT ->
      pure $ Schema.Int
    StringT ->
      pure . Schema.Nested . Schema.Binary . Just $ Table.Utf8
    UnitT ->
      pure $ Schema.Unit
    ErrorT ->
      pure $ Schema.Int
    FactIdentifierT ->
      pure $ Schema.Int
    ArrayT e ->
      Schema.Nested . Schema.Array <$> zebraColumnSchema e
    MapT k v ->
      fmap Schema.Nested . Schema.Map <$> zebraColumnSchema k <*> zebraColumnSchema v
    OptionT e -> do
      c <- zebraColumnSchema e
      pure . Schema.Enum $ Cons.from2 (Table.Variant "none" c) (Table.Variant "some" c)
    PairT a b ->
      fmap Schema.Struct . Cons.from2
        <$> (Table.Field "fst" <$> zebraColumnSchema a)
        <*> (Table.Field "snd" <$> zebraColumnSchema b)
    SumT a b ->
      fmap Schema.Enum . Cons.from2
        <$> (Table.Variant "left"  <$> zebraColumnSchema a)
        <*> (Table.Variant "right" <$> zebraColumnSchema b)
    StructT st
      | (f:fs) <- Map.toList $ getStructType st
      -> let
           fieldOf (k,v) =
             Table.Field
               (Table.FieldName . nameOfStructField $ k) <$> zebraColumnSchema v
         in do
           f' <- fieldOf f
           fs' <- mapM fieldOf fs
           pure . Schema.Struct . Cons.fromNonEmpty $ f' :| fs'
      | otherwise ->
          Left (ZebraUnexpectedStruct st)
    BufT _ e ->
      Schema.Nested . Schema.Array <$> zebraColumnSchema e


