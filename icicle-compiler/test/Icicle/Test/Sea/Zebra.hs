{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Test.Sea.Zebra where

import           Control.Monad.Catch (bracket)
import           Control.Monad.IO.Class (liftIO)

import           Data.String
import qualified Data.List as List
import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Text.Show.Pretty (ppShow)

import           Foreign
import           Foreign.C.String

import           System.IO
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Posix as Posix

import qualified Prelude as Savage

import           P

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, firstEitherT, bracketEitherT', runEitherT, hoistMaybe)

import           Disorder.Core.IO (testIO)
import           Disorder.Jack (Property, Jack)
import           Disorder.Jack (gamble, arbitrary, (===), justOf, vectorOf, suchThat, counterexample)

import           Jetski

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool
import           Anemone.Foreign.Segv (withSegv)

import qualified Test.Zebra.Jack as Zebra
import qualified Zebra.Data.Core as Zebra
import qualified Zebra.Data.Entity as Zebra
import qualified Zebra.Foreign.Entity as Zebra
import           Zebra.Schema (Schema)
import qualified Zebra.Schema as Schema
import           Zebra.Table (Table(..), TableError)
import qualified Zebra.Table as Table
import qualified Zebra.Value as Zebra (Value)
import qualified Zebra.Value as Value

import qualified Icicle.Internal.Pretty as PP
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Data
import qualified Icicle.Data.Time as Icicle
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO
import           Icicle.Sea.Eval.Base
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary
import qualified Icicle.Test.Foreign.Utils as Test


--
-- Reading a (decoded) Zebra entity into an Icicle input struct
--
prop_read_entity :: Property
prop_read_entity =
  gamble jZebraChunkFactCount $ \chunkFactCount ->
  gamble jZebraWellTyped $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      wt =
        zWellTyped $ zwt
      hasFacts =
        fmap (atFact . snd) $ List.zip (wtEntities wt) (wtFacts wt)
    Result inputs dropped <- runTest pool chunkFactCount testAllocLimitBytes zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Dropped Entities = " <> ppShow dropped)
           $ counterexample ("Facts = " <> ppShow (zFacts zwt))
           $ concatMap snd inputs === hasFacts

--
-- Each entity is either read or dropped if they exceed the allocation limit
--
prop_read_or_drop_entity :: Property
prop_read_or_drop_entity =
  gamble jZebraChunkFactCount $ \chunkFactCount ->
  gamble jZebraAllocLimitBytes $ \allocLimitBytes ->
  gamble jZebraWellTyped $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      wt =
        zWellTyped $ zwt
      hasFacts =
        fmap fst $ List.zip (wtEntities wt) (wtFacts wt)
    Result inputs dropped <- runTest pool chunkFactCount allocLimitBytes zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Dropped Entities = " <> ppShow dropped)
           $ counterexample ("Facts = " <> ppShow (zFacts zwt))
           $ List.sort (fmap fst inputs <> dropped) === List.sort hasFacts


data Result = Result {
    resultRead :: [(Entity, [BaseValue])]
  , resultDropped :: [Entity]
  } deriving (Eq, Show)

instance Monoid Result where
  mempty =
    Result mempty mempty
  mappend (Result xs ys) (Result as bs) =
    Result (xs <> as) (ys <> bs)

runTest ::
     Mempool
  -> ZebraChunkFactCount
  -> Int
  -> ZebraWellTyped
  -> EitherT SeaError IO Result
runTest pool chunk_step alloc_limit_bytes zwt@(ZebraWellTyped wt facts entities) =
  join . liftIO . liftM hoistEither . withSegv (pp chunk_step alloc_limit_bytes zwt) $ do
    results <- forM entities $ \(ty, entity) -> do
      c_entity <- Zebra.foreignOfEntity pool entity
      runEitherT $ do
        code <- hoistEither $ codeOf wt
        opts <- getCompilerOptions

        withSystemTempDirectory "zebra-test-" $ \dir -> let drop_fp = dir <> "/drop.txt" in
          withWritableFd drop_fp $ \drop_fd ->
            withSeaLibrary opts code $ \src -> do
              init <- firstEitherT SeaJetskiError $ function src "zebra_alloc_state" (retPtr retVoid)
              finish <- firstEitherT SeaJetskiError $ function src "zebra_collect_state" (retPtr retVoid)
              test_read_entity <- firstEitherT SeaJetskiError $ function src "test_zebra_read_entity" (retPtr retWord8)
              test_fleet <- firstEitherT SeaJetskiError $ function src "test_setup_fleet" (retPtr retVoid)
              test_check_limit <- firstEitherT SeaJetskiError $ function src "test_check_limit" Test.retBool

              withWords zebraConfigCount $ \config -> do
                pokeWordOff config zebraConfigDropFd drop_fd
                pokeWordOff config zebraConfigOutputBufferSize defaultPsvOutputBufferSize
                pokeWordOff config zebraConfigChunkFactCount (unZebraChunkFactCount chunk_step)
                pokeWordOff config zebraConfigAllocLimitBytes alloc_limit_bytes

                bracketEitherT'
                  (liftIO (init [ argPtr nullPtr, argPtr config, argInt64 1 ]))
                  (\state -> (liftIO (finish [ argPtr config, argPtr state ])))
                  (\state -> do
                     fleet_ptr <- peekWordOff state zebraStateFleet
                     struct_count <- inputFieldsCount <$>
                       hoistEither (stateOfPrograms 0 (wtAttribute wt) (wtAvalancheFlat wt :| []))
                     fact_count <- hoistMaybe (SeaZebraError "test_impossible") . fmap length $
                       Map.lookup (Entity . Text.decodeUtf8 . Zebra.unEntityId . Zebra.entityId $ entity) facts


                     let
                       step =
                         unZebraChunkFactCount chunk_step

                       chunk_lengths =
                         List.replicate (fact_count `div` step) step <> [ fact_count `rem` step ]

                       entity_id =
                         ByteString.unpack . Zebra.unEntityId . Zebra.entityId $ entity

                     read_inputs <- forM chunk_lengths $ \len -> do
                       dropped <- liftIO . withCStringLen entity_id $ \(id_bytes, id_length) -> do
                           e1 <- test_fleet
                                   [ argPtr id_bytes
                                   , argInt32 (fromIntegral id_length)
                                   , argPtr nullPtr
                                   , argPtr fleet_ptr ]
                           when (e1 /= nullPtr) $
                             fail "failed to configure fleet"

                           e2 <- test_read_entity
                                   [ argPtr nullPtr
                                   , argPtr state
                                   , argPtr (Zebra.unCEntity c_entity) ]
                           when (e2 /= nullPtr) $ do
                             err <- peekCString (castPtr e2)
                             fail $ "failed to read entity: " <> err

                           x <- test_check_limit
                                  [ argPtr state
                                  , argPtr (Zebra.unCEntity c_entity) ]
                           return x

                       let
                         saveIt
                           | dropped =
                               return Nothing
                           | otherwise = do
                               x <- saveInputs ty struct_count len fleet_ptr
                               return . Just $ (Entity . Text.pack $ entity_id, x)

                       saveIt

                     dropped_entity <- liftIO (fmap Entity . List.nub . Text.lines <$> Text.readFile drop_fp)

                     let
                       inputs = catMaybes read_inputs

                     return (Result inputs dropped_entity)
                  )
    return . fmap mconcat . sequence $ results

saveInputs :: ValType -> Int -> Int -> Ptr a -> EitherT SeaError IO [BaseValue]
saveInputs ty struct_count fact_count fleet_ptr =
  bracketEitherT' (liftIO $ mallocBytes (struct_count * 8)) (liftIO . free) $ \buf -> do
    program_ptr <- peekWordOff fleet_ptr (fleetProgramOf 0)
    tombstones_ptr <- peekWordOff program_ptr programInputError

    let
      input_start
        = programInputStart

      -- slice out the input fields at the index (kind of like a transpose)
      -- e.g slice out the second fact:
      -- src:
      --           fact_1 fact_2 ...
      --   field_1        A
      --   field_2        B
      --   ...
      -- dst:
      --   [ A, B, ... ]
      --
      slice dst fact_i =
        forM_ [0 .. struct_count - 1] $ \field_i -> do
          ptr_head <- peekWordOff program_ptr (input_start + field_i)
          let ptr_src = plusPtr ptr_head (fact_i  * 8)
              ptr_dst = plusPtr dst      (field_i * 8)
          copyBytes ptr_dst ptr_src 8

      peekInputs xs index
        | index == fact_count = return xs
        | otherwise = do
            tombstone <- liftIO $ peekWordOff tombstones_ptr index

            x <- case errorOfWord tombstone of
               ExceptNotAnError -> do
                 liftIO $ slice buf index
                 VRight . snd <$> peekOutput buf 0 ty
               e ->
                 pure (VLeft (VError e))

            peekInputs (x:xs) (index + 1)

    List.reverse <$> peekInputs [] 0

--------------------------------------------------------------------------------

data TestError
  = ZebraError (TableError Schema)
  | UnexpectedError ValType [BaseValue]
  deriving (Show)

data ZebraWellTyped = ZebraWellTyped {
    zWellTyped    :: WellTyped
  , zFacts        :: Map Entity [BaseValue]
  -- ^ facts grouped by entity
  , zEntity       :: [(ValType, Zebra.Entity Schema)]
  -- ^ zebra entity and type where wtFactType = Sum Error type
  }

instance Show ZebraWellTyped where
  show z =
    let
      wt = zWellTyped z
    in
      "Entities = " <> show (wtEntities wt) <> "\n" <>
      "Fact type = " <> show (wtFactType wt) <> "\n" <>
      "Facts = " <> ppShow (wtFacts wt) <> "\n" <>
      -- "As Zebra values = \n" <> ppShow rows <> "\n" <>
      "As Zebra entities = \n" <> ppShow (zEntity z) <>
      -- "Avalanche program = \n" <> show (PP.pretty (wtAvalancheFlat wt)) <>
      "\n"

jZebraChunkFactCount :: Jack ZebraChunkFactCount
jZebraChunkFactCount = ZebraChunkFactCount <$>
  arbitrary `suchThat` (>= 1)

-- Use bytes instead of GBs for testing
jZebraAllocLimitBytes :: Jack Int
jZebraAllocLimitBytes =
  arbitrary `suchThat` (> 0)

jZebraWellTyped :: Jack ZebraWellTyped
jZebraWellTyped = justOf $ do
  -- we don't read arrays (except of bytes) in zebra
  let
    supportedInputType x =
      case wtFactType x of
        SumT _ (ArrayT _) -> False
        _ -> True
  wt <- arbitrary `suchThat` supportedInputType
  zebraOfWellTyped wt

-- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
-- let ts = fmap (Zebra.Time . fromIntegral . Icicle.secondsCountJulian . atTime) (wtFacts wt)
zebraOfWellTyped :: WellTyped -> Jack (Maybe ZebraWellTyped)
zebraOfWellTyped wt@(WellTyped entities _ ty facts _ _ _ _)= do
  let
    ins (k, x) m =
      Map.insertWith (<>) k [atFact x] m

    grouped =
      foldr ins Map.empty (List.zip entities facts)

  xs <- forM (Map.toList grouped) $ \(entity, values) -> do
    case zebraOfFacts ty values of
      Left e ->
        Savage.error (show e)
      Right Nothing ->
        return Nothing
      Right (Just (t, tombstones, _, table)) -> do
        let
          e =
            Zebra.EntityId (Text.encodeUtf8 (getEntity entity))

          n =
            length values

        attribute <- Zebra.Attribute
          <$> pure (Storable.fromList (List.replicate n 0))
          <*> (Storable.fromList <$> vectorOf n Zebra.jFactsetId)
          <*> pure (Storable.fromList tombstones)
          <*> pure table

        return . Just $ (t, Zebra.Entity (Zebra.hashEntityId e) e (Boxed.singleton attribute))

  return (ZebraWellTyped wt grouped <$> sequence xs)


-- we are not reading nested arrays in zebra right now
schemaOfType' :: ValType -> Maybe Schema
schemaOfType' ty = case ty of
  ArrayT {} ->
    Nothing
  BufT {} ->
    Nothing
  MapT {} ->
    Nothing
  _ ->
    schemaOfType ty

schemaOfType :: ValType -> Maybe Schema
schemaOfType ty = case ty of
  BoolT ->
    pure Schema.bool

  TimeT ->
    pure Schema.Int

  DoubleT ->
    pure Schema.Double

  IntT ->
    pure Schema.Int

  StringT ->
    pure $ Schema.Array Schema.Byte

  ErrorT ->
    pure Schema.Int

  UnitT ->
    pure $ Schema.Struct Boxed.empty

  FactIdentifierT ->
    pure Schema.Int

  ArrayT t ->
    Schema.Array <$> schemaOfType' t

  BufT _ t ->
    Schema.Array <$> schemaOfType' t

  PairT a b -> do
    a' <- schemaOfType a
    b' <- schemaOfType b
    pure . Schema.Struct . Boxed.fromList $
      [ Schema.Field (Schema.FieldName "fst") a'
      , Schema.Field (Schema.FieldName "snd") b' ]

  OptionT t ->
    Schema.option <$> schemaOfType t

  SumT a b ->
    let
      leftOf x =
        Schema.Variant (Schema.VariantName "left")  <$> schemaOfType x
      rightOf x =
        Schema.Variant (Schema.VariantName "right") <$> schemaOfType x
    in Schema.Enum <$> leftOf a <*> (Boxed.singleton <$> rightOf b)

  MapT k v -> do
    k' <- schemaOfType' k
    v' <- schemaOfType' v
    pure . Schema.Struct . Boxed.fromList $
      [ Schema.Field (Schema.FieldName "keys") (Schema.Array k')
      , Schema.Field (Schema.FieldName "vals") (Schema.Array v') ]

  StructT struct
    -- An empty icicle struct is not unit (unlike zebra), it has no input variable.
    | fields <- getStructType struct
    , not (Map.null fields) ->
        let
          fieldOf (f, t) =
            Schema.Field (Schema.FieldName (nameOfStructField f)) <$> schemaOfType t
        in Schema.Struct . Boxed.fromList <$> mapM fieldOf (Map.toList fields)

  _ -> Nothing

zebraOfValue :: ValType -> BaseValue -> Either TestError Zebra.Value
zebraOfValue ty val = case val of
  VInt x ->
    pure . Value.Int . fromIntegral $ x

  VDouble x ->
    pure . Value.Double $ x

  VUnit ->
    pure . Value.Struct $ Boxed.empty

  VBool False ->
    pure $ Value.false

  VBool True ->
    pure $ Value.true

  VTime x ->
    pure . Value.Int . fromIntegral . Icicle.packedOfTime $ x

  VString x ->
    pure . Value.ByteArray . Text.encodeUtf8 $ x

  VArray xs
    | ArrayT t <- ty
    -> Value.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VPair a b
    | PairT ta tb <- ty
    -> do a' <- zebraOfValue ta a
          b' <- zebraOfValue tb b
          pure . Value.Struct . Boxed.fromList $ [a', b']

  VLeft x
    | SumT t _ <- ty
    -> Value.Enum 0 <$> zebraOfValue t x

  VRight x
    | SumT _ t <- ty
    -> Value.Enum 1 <$> zebraOfValue t x

  VNone
    | OptionT _ <- ty
    -> pure $ Value.none

  VSome x
    | OptionT t <- ty
    -> Value.some <$> zebraOfValue t x

  VMap x
    | MapT tk tv <- ty
    -> do keys <- mapM (zebraOfValue tk) (Map.keys x)
          vals <- mapM (zebraOfValue tv) (Map.elems x)
          pure . Value.Struct . Boxed.fromList $
            [ Value.Array (Boxed.fromList keys), Value.Array (Boxed.fromList vals) ]
  VStruct xs
    | StructT struct <- ty
    , types <- getStructType struct
    , vs <- Map.elems (Map.intersectionWith (,) types xs)
    , length vs == Map.size types
    -> Value.Struct . Boxed.fromList <$> mapM (uncurry zebraOfValue) vs

  VBuf xs
    | BufT _ t <- ty
    -> Value.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VFactIdentifier x ->
    pure . Value.Int . fromIntegral . getFactIdentifierIndex $ x

  VError ExceptTombstone ->
    Left (UnexpectedError ty [val])

  VError e ->
    pure . Value.Int . fromIntegral . wordOfError $ e

  _ ->
    Left (UnexpectedError ty [val])

zebraOfTopValue :: ValType -> BaseValue -> Either TestError (Zebra.Tombstone, Zebra.Value)
zebraOfTopValue t val
  | VRight v <- val
  = (Zebra.NotTombstone,) <$> zebraOfValue t v

  | VLeft (VError ExceptTombstone) <- val
  = (Zebra.Tombstone,) <$> zebraOfValue t (defaultOfType t)

  | otherwise
  = Left (UnexpectedError t [val])

zebraOfFacts ::
      ValType
  -> [BaseValue]
  -> Either TestError (Maybe (ValType, [Zebra.Tombstone], [Zebra.Value], Table Schema))
zebraOfFacts ty facts
  | SumT ErrorT t <- ty =
    case schemaOfType t of
      Nothing ->
        pure Nothing
      Just schema -> do
        (tombstones, rows) <- List.unzip <$> mapM (zebraOfTopValue t) facts
        tables <- first ZebraError $ mapM (Table.fromRow schema) rows
        table <- first ZebraError
               $ if null tables
                 then pure $ Table.empty schema
                 else Table.concat . Boxed.fromList $ tables
        pure . Just $ (t, tombstones, rows, table)

  | otherwise = Left (UnexpectedError ty facts)

--------------------------------------------------------------------------------

testAllocLimitBytes :: Int
testAllocLimitBytes = 10 * 1024 * 1024 * 1024

testSnapshotTime :: Time
testSnapshotTime = Icicle.unsafeTimeOfYMD 9999 1 1

codeOf :: WellTyped -> Either SeaError SourceCode
codeOf wt = do
  let
    dummy =
      HasInput
        (FormatZebra
          (ZebraConfig (ZebraChunkFactCount 0) (ZebraAllocLimitGB 0))
          (Snapshot testSnapshotTime)
          (PsvOutputConfig (Snapshot testSnapshotTime) PsvOutputDense defaultOutputMissing))
        (InputOpts AllowDupTime Map.empty)
        ("" :: String)
    attr = wtAttribute wt
    flat = wtAvalancheFlat wt :| []

  src <- codeOfPrograms dummy [attr] [(attr, flat)]

  pure . textOfDoc . PP.vsep $
    [ PP.pretty src
    , ""
    , "ierror_loc_t test_setup_fleet (const char *entity, size_t size, piano_t *piano, ifleet_t *fleet) {"
    , "    psv_collect_fleet (fleet);"
    , "    return psv_configure_fleet (entity, size, piano, fleet);"
    , "}"
    , ""
    , "int64_t piano_max_count (piano_t *piano) {"
    , "    return 1;"
    , "}"
    , ""
    , "error_t piano_lookup (piano_t *piano, const uint8_t *needle_id, size_t needle_id_size, int64_t *out_count, const int64_t **out_times) {"
    , "    return 0;"
    , "}"
    , ""
    , "ierror_msg_t test_zebra_read_entity (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity) {"
    , "    if (!zebra_limit_exceeded (state)) {"
    , "        return zebra_read_entity (piano, state, entity);"
    , "    }"
    , "    return 0;"
    , "}"
    , ""
    , "int64_t test_check_limit (zebra_state_t *state, zebra_entity_t *entity) {"
    , "    if (zebra_limit_exceeded (state)) {"
    , "        zebra_write_dropped_entity (state, entity);"
    , "        return 1;"
    , "    }"
    , "    return 0;"
    , "}"
    , ""
    ]

pp :: ZebraChunkFactCount -> Int -> ZebraWellTyped -> String
pp size limit wt =
  "=== Entity ===\n" <>
  "Zebra chunk size = " <> show (unZebraChunkFactCount size) <> " facts\n" <>
  "Zebra alloc limit = " <> show limit  <> " bytes\n" <>
  show wt

withSystemTempDirectory :: FilePath -> (FilePath -> EitherT SeaError IO a) -> EitherT SeaError IO a
withSystemTempDirectory template action = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release action

withWritableFd :: FilePath -> (Posix.Fd -> EitherT SeaError IO a) -> EitherT SeaError IO a
withWritableFd path =
  bracketEitherT' (liftIO $ Posix.createFile path (Posix.CMode 0O644))
                  (liftIO . Posix.closeFd)

withSeaLibrary :: [CompilerOption] -> SourceCode -> (Library -> EitherT SeaError IO a) -> EitherT SeaError IO a
withSeaLibrary opts code =
  bracketEitherT'
    (firstEitherT SeaJetskiError $ compileLibrary NoCacheLibrary opts code)
    (firstEitherT SeaJetskiError . releaseLibrary)

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunNormal checkArgs

