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
import qualified X.Data.Vector.Cons as Cons

import           Disorder.Core.IO (testIO)
import           Disorder.Jack (Property, Jack)
import           Disorder.Jack (gamble, arbitrary, (===), justOf, vectorOf, counterexample)

import           Jetski

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool
import           Anemone.Foreign.Segv (withSegv)

import qualified Test.Zebra.Jack as Zebra

import qualified Zebra.Foreign.Entity as Zebra
import qualified Zebra.Factset.Data as Zebra
import qualified Zebra.Factset.Entity as Zebra
import qualified Zebra.Table.Encoding as Zebra
import qualified Zebra.Table.Striped as Striped
import qualified Zebra.Table.Logical as Logical
import qualified Zebra.Table.Data as Schema
import qualified Zebra.Table.Schema as Schema

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
  gamble jZebraWellTyped $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      wt =
        zWellTyped $ zwt
      hasFacts =
        fmap (atFact . snd) $ List.zip (wtEntities wt) (wtFacts wt)
    Result inputs <- runTest pool zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Facts = " <> ppShow (zFacts zwt))
           $ concatMap snd inputs === hasFacts

--
-- Each entity is either read or dropped if they exceed the allocation limit
--
zprop_read_or_drop_entity :: Property
zprop_read_or_drop_entity =
  gamble jZebraWellTyped $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      wt =
        zWellTyped $ zwt
      hasFacts =
        fmap fst $ List.zip (wtEntities wt) (wtFacts wt)
    Result inputs <- runTest pool zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Facts = " <> ppShow (zFacts zwt))
           $ List.sort (fmap fst inputs) === List.sort hasFacts


data Result = Result {
    resultRead :: [(Entity, [BaseValue])]
  } deriving (Eq, Show)

instance Monoid Result where
  mempty =
    Result mempty
  mappend (Result xs) (Result as) =
    Result (xs <> as)

runTest ::
     Mempool
  -> ZebraWellTyped
  -> EitherT SeaError IO Result
runTest pool zwt@(ZebraWellTyped wt facts entities) =
  join . liftIO . liftM hoistEither . withSegv (pp zwt) $ do
    results <- forM entities $ \(ty, entity) -> do
      c_entity <- Zebra.foreignOfEntity pool entity
      runEitherT $ do
        code <- hoistEither $ codeOf wt
        opts <- getCompilerOptions
        let opts' = ["-DICICLE_ASSERT=1", "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=" <> Text.pack (show (100 * length facts)) ] <> opts

        withSystemTempDirectory "zebra-test-" $ \dir -> let drop_fp = dir <> "/drop.txt" in
          withWritableFd drop_fp $ \drop_fd ->
            withSeaLibrary opts' code $ \src -> do
              init <- firstEitherT SeaJetskiError $ function src "zebra_alloc_state" (retPtr retVoid)
              finish <- firstEitherT SeaJetskiError $ function src "zebra_collect_state" (retPtr retVoid)
              test_read_entity <- firstEitherT SeaJetskiError $ function src "test_zebra_read_entity" (retPtr retWord8)
              test_fleet <- firstEitherT SeaJetskiError $ function src "test_setup_fleet" (retPtr retVoid)

              withWords zebraConfigCount $ \config -> do
                pokeWordOff config zebraConfigDropFd drop_fd
                pokeWordOff config zebraConfigOutputBufferSize defaultPsvOutputBufferSize

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
                       entity_id =
                         ByteString.unpack . Zebra.unEntityId . Zebra.entityId $ entity

                     read_inputs <- do
                       liftIO . withCStringLen entity_id $ \(id_bytes, id_length) -> do
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

                         pure ()

                       let
                         saveIt = do
                           x <- saveInputs ty struct_count fact_count fleet_ptr
                           return (Entity . Text.pack $ entity_id, x)

                       saveIt

                     let
                       inputs = [read_inputs]

                     return (Result inputs)
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
  = ZebraError Striped.StripedError
  | UnexpectedError ValType [BaseValue]
  deriving (Show)

data ZebraWellTyped = ZebraWellTyped {
    zWellTyped    :: WellTyped
  , zFacts        :: Map Entity [BaseValue]
  -- ^ facts grouped by entity
  , zEntity       :: [(ValType, Zebra.Entity)]
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

jZebraWellTyped :: Jack ZebraWellTyped
jZebraWellTyped = justOf $ do
  wt <- arbitrary
  zebraOfWellTyped wt

-- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
-- let ts = fmap (Zebra.Time . fromIntegral . Icicle.secondsCountJulian . atTime) (wtFacts wt)
zebraOfWellTyped :: WellTyped -> Jack (Maybe ZebraWellTyped)
zebraOfWellTyped wt@(WellTyped entities _ ty facts _ _ _ _ _)= do
  let
    ins (k, x) m =
      Map.insertWith (<>) k [atFact x] m

    -- group facts by entity
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

schemaOfType :: ValType -> Maybe Schema.Column
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
    pure . Schema.Nested . Schema.Binary . Just $ Zebra.Utf8

  ErrorT ->
    pure Schema.Int

  UnitT ->
    pure $ Schema.Unit

  FactIdentifierT ->
    pure Schema.Int

  ArrayT t ->
    Schema.Nested . Schema.Array <$> schemaOfType t

  BufT _ t ->
    Schema.Nested . Schema.Array <$> schemaOfType t

  PairT a b -> do
    a' <- schemaOfType a
    b' <- schemaOfType b
    pure . Schema.Struct $ Cons.from2
      (Schema.Field (Schema.FieldName "fst") a')
      (Schema.Field (Schema.FieldName "snd") b')

  OptionT t ->
    Schema.option <$> schemaOfType t

  SumT a b ->
    let
      leftOf x =
        Schema.Variant (Schema.VariantName "left")  <$> schemaOfType x
      rightOf x =
        Schema.Variant (Schema.VariantName "right") <$> schemaOfType x
    in do
      a' <- leftOf a
      b' <- rightOf b
      pure . Schema.Enum $ Cons.from2 a' b'

  MapT k v -> do
    k' <- schemaOfType k
    v' <- schemaOfType v
    pure . Schema.Nested $ Schema.Map k' v'

  StructT struct
    -- structs must have at leaast one field in both zebra and icicle
    | (f:fs) <- Map.toList (getStructType struct) ->
      let
        fieldOf (x, t) =
          Schema.Field (Schema.FieldName (nameOfStructField x)) <$> schemaOfType t
      in do
        f' <- fieldOf f
        fs' <- mapM fieldOf fs
        pure . Schema.Struct . Cons.fromNonEmpty $ f' :| fs'

  _ -> Nothing

zebraOfValue :: ValType -> BaseValue -> Either TestError Logical.Value
zebraOfValue ty val = case val of
  VInt x ->
    pure . Logical.Int . fromIntegral $ x

  VDouble x ->
    pure . Logical.Double $ x

  VUnit ->
    pure $ Logical.Unit

  VBool False ->
    pure $ Logical.false

  VBool True ->
    pure $ Logical.true

  VTime x ->
    pure . Logical.Int . fromIntegral . Icicle.packedOfTime $ x

  VString x ->
    pure . Logical.Nested . Logical.Binary . Text.encodeUtf8 $ x

  VArray xs
    | ArrayT t <- ty
    -> Logical.Nested . Logical.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VPair a b
    | PairT ta tb <- ty
    -> do a' <- zebraOfValue ta a
          b' <- zebraOfValue tb b
          pure . Logical.Struct $ Cons.from2 a' b'

  VLeft x
    | SumT t _ <- ty
    -> Logical.Enum 0 <$> zebraOfValue t x

  VRight x
    | SumT _ t <- ty
    -> Logical.Enum 1 <$> zebraOfValue t x

  VNone
    | OptionT _ <- ty
    -> pure $ Logical.none

  VSome x
    | OptionT t <- ty
    -> Logical.some <$> zebraOfValue t x

  VMap x
    | MapT tk tv <- ty
    , ks <- Map.keys x
    , vs <- Map.elems x
    -> do ks' <- mapM (zebraOfValue tk) ks
          vs' <- mapM (zebraOfValue tv) vs
          pure . Logical.Nested . Logical.Map . Map.fromList $ List.zip ks' vs'

  VStruct xs
    | StructT struct <- ty
    , types <- getStructType struct
    , (v:vs) <- Map.elems (Map.intersectionWith (,) types xs)
    -> do v' <- uncurry zebraOfValue $ v
          vs' <- mapM (uncurry zebraOfValue) vs
          pure . Logical.Struct . Cons.fromNonEmpty $ v' :| vs'

  VBuf xs
    | BufT _ t <- ty
    -> Logical.Nested . Logical.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VFactIdentifier x ->
    pure . Logical.Int . fromIntegral . getFactIdentifierIndex $ x

  VError ExceptTombstone ->
    Left (UnexpectedError ty [val])

  VError e ->
    pure . Logical.Int . fromIntegral . wordOfError $ e

  _ ->
    Left (UnexpectedError ty [val])

zebraOfTopValue :: ValType -> BaseValue -> Either TestError (Zebra.Tombstone, Logical.Value)
zebraOfTopValue t val
  | VRight v <- val
  = (Zebra.NotTombstone,) <$> zebraOfValue t v

  | VLeft (VError ExceptTombstone) <- val
  = (Zebra.Tombstone,) <$> zebraOfValue t (defaultOfType t)

  | otherwise
  = Left (UnexpectedError t [val])

-- | Convert rows of facts into zebra logical tables.
zebraOfFacts ::
      ValType
  -> [BaseValue]
  -> Either TestError (Maybe (ValType, [Zebra.Tombstone], [Logical.Value], Striped.Table))
zebraOfFacts ty facts
  | SumT ErrorT t <- ty =
    case schemaOfType t of
      Nothing ->
        pure Nothing
      Just schema -> do
        (tombstones, rows) <- List.unzip <$> mapM (zebraOfTopValue t) facts
        rows' <- first ZebraError . Striped.fromValues schema . Boxed.fromList $ rows
        let table = Striped.Array rows'
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
          (ZebraConfig (wtMaxMapSize wt))
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
    , "error_t piano_lookup (piano_t *piano, const uint8_t *needle_id, size_t needle_id_size, int64_t *out_count, const int64_t **out_label_times, const int64_t **out_label_name_offsets, const int64_t **out_label_name_lengths, const uint8_t **out_label_name_data) {"
    , "    return 0;"
    , "}"
    , ""
    , "ierror_msg_t test_zebra_read_entity (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity) {"
    , "    return zebra_read_entity (piano, state, entity);"
    , "}"
    ]

pp :: ZebraWellTyped -> String
pp wt =
  "=== Entity ===\n" <>
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

