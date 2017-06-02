{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

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

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, firstEitherT, bracketEitherT', runEitherT)
import qualified X.Data.Vector.Cons as Cons

import           Disorder.Core.IO (testIO)
import           Disorder.Jack (Property, Jack)
import           Disorder.Jack (gamble, arbitrary, (===), vectorOf, suchThat, counterexample, discard)

import           Jetski

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool
import           Anemone.Foreign.Segv (withSegv)

import qualified Test.Zebra.Jack as Zebra
import           Test.QuickCheck (forAll, Gen)

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
import           Icicle.Common.Eval
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
  forAll (gWellTyped ty) $ \wt ->
  gamble (jZebraWellTyped wt) $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      expected =
        fmap (List.concat . fmap snd) .
        Map.mapKeys (Entity . Text.decodeUtf8 . Zebra.unEntityId . Zebra.entityId) .
        zFacts $
          zwt
    Result inputs dropped <- runTest pool zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Dropped Entities = " <> ppShow dropped)
           $ inputs === expected

--
-- Each entity is either read or dropped if they exceed the allocation limit
--
prop_read_or_drop_entity :: Property
prop_read_or_drop_entity =
  gamble jInputType $ \ty ->
  forAll (gWellTyped ty) $ \wt ->
  gamble (jZebraWellTyped wt) $ \zwt ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> Test.runRight $ do
    let
      entities =
        fmap (Entity . Text.decodeUtf8 . Zebra.unEntityId . Zebra.entityId) . Map.keys . zFacts $ zwt
    Result inputs dropped <- runTest pool chunkFactCount allocLimitBytes zwt
    return $ counterexample ("Read Inputs = " <> ppShow inputs)
           $ counterexample ("Dropped Entities = " <> ppShow dropped)
           $ List.sort ((fmap fst . Map.toList $ inputs) <> dropped) === List.sort entities


data Result = Result {
    resultRead :: Map Entity [BaseValue]
  , resultDropped :: [Entity]
  } deriving (Eq, Show)

instance Monoid Result where
  mempty =
    Result mempty mempty
  mappend (Result xs ys) (Result as bs) =
    Result (Map.unionWith (<>) xs as) (ys <> bs)

runTest ::
     Mempool
  -> ZebraWellTyped
  -> EitherT SeaError IO Result
runTest pool zwt@(ZebraWellTyped wt facts entities) =
 = join . liftIO . fmap hoistEither . withSegv (pp zwt) $ do
    let
      step =
        unZebraChunkFactCount chunk_step

      result (Left e) =
        Result Map.empty [e]
      result (Right (e,v)) =
        Result (Map.singleton e v) []

    runEitherT . fmap (mconcat . fmap result) . forM (Map.toList entities) $ \(entity, attributes) -> do
      let
        entity_id =
          ByteString.unpack . Zebra.unEntityId . Zebra.entityId $ entity
        entity_text =
          Entity . Text.pack $ entity_id
        fact_count =
          length . List.concat . fmap snd $ attributes
        last_chunk
          | r <- fact_count `rem` step
          , r /= 0
          = [r]
          | otherwise
          = []
        chunk_lengths =
            List.replicate (fact_count `div` step) step <> last_chunk


      bracketEitherT'
        (liftIO . newCStringLen $ entity_id)
        (liftIO . free . fst) $ \(id_bytes, id_length) ->
        fmap (maybe (Left entity_text) (Right . (entity_text,))) $ do
          c_entity <- Zebra.foreignOfEntity pool entity
          code <- hoistEither $ codeOf wt
          opts <- getCompilerOptions
          let
            opts' =
              ["-DICICLE_ASSERT=1", "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=" <> Text.pack "1000000"] <> opts

          withSystemTempDirectory "zebra-test-" $ \dir -> do
            let
              drop_fp =
                dir <> "/drop.txt"

            withWritableFd drop_fp $ \drop_fd ->
              withSeaLibrary opts' code $ \src -> do

                init <- firstEitherT SeaJetskiError $
                  function src "zebra_alloc_state" (retPtr retVoid)
                finish <- firstEitherT SeaJetskiError $
                  function src "zebra_collect_state" (retPtr retVoid)
                test_read_entity <- firstEitherT SeaJetskiError $
                  function src "test_zebra_read_entity" (retPtr retWord8)
                test_fleet <- firstEitherT SeaJetskiError $
                  function src "test_setup_fleet" (retPtr retVoid)

                withWords zebraConfigCount $ \config -> do
                  pokeWordOff config zebraConfigDropFd drop_fd
                  pokeWordOff config zebraConfigOutputBufferSize defaultPsvOutputBufferSize
                  pokeWordOff config zebraConfigChunkFactCount (unZebraChunkFactCount chunk_step)
                  pokeWordOff config zebraConfigAllocLimitBytes alloc_limit_bytes

                  bracketEitherT'
                    (liftIO (init [ argPtr nullPtr, argPtr config, argInt64 1 ]))
                    (\state -> (liftIO (finish [ argPtr config, argPtr state ])))
                    (\state -> do
                       fleet_ptr <- liftIO $ peekWordOff state zebraStateFleet
                       groupedByChunk <- forM (List.replicate (length chunk_lengths) ()) $ \_ -> do
                         dropped <- do
                             e1 <- liftIO $ test_fleet
                                     [ argPtr id_bytes
                                     , argInt32 (fromIntegral id_length)
                                     , argPtr nullPtr
                                     , argPtr fleet_ptr ]
                             when (e1 /= nullPtr) $
                               fail "failed to configure fleet"

                             e2 <- liftIO $ test_read_entity
                                     [ argPtr nullPtr
                                     , argPtr state
                                     , argPtr (Zebra.unCEntity c_entity) ]
                             when (e2 /= nullPtr) $ do
                               err <- liftIO $ peekCString (castPtr e2)
                               fail $ "failed to read entity: " <> err

                             x <- liftIO $ test_check_limit
                                    [ argPtr state
                                    , argPtr (Zebra.unCEntity c_entity) ]
                             return x

                         if dropped
                         then
                           return Nothing
                         else
                           Just <$> saveInputs fleet_ptr (fmap fst $ attributes) programs

                       let
                         groupedByAttribute =
                           fmap (List.transpose) . sequence $ groupedByChunk

                       return . fmap (List.concat . fmap (List.concat . fmap snd)) $ groupedByAttribute
                    )

-- for each attribute, read a `new_fact_count` number of facts
saveInputs ::
     Ptr a
  -> [(Attribute, ValType)]
  -> [WellTypedAttribute]
  -> EitherT SeaError IO [(Attribute, [BaseValue])]
saveInputs fleet_ptr attributes programs =
  forM (List.zip [0..] attributes) $ \(index, (attribute, ty)) -> do
    case List.find ((== attribute) . wtAttribute) programs of
      Nothing ->
        hoistEither . Left . SeaProgramNotFound $ attribute
      Just (WellTypedAttribute _ _ _ _ _ flat) -> do
        struct_count <-
          inputFieldsCount <$>
            hoistEither (stateOfPrograms index attribute (flat :| []))
        x <- bracketEitherT' (liftIO $ mallocBytes (struct_count * 8)) (liftIO . free) $ \buf -> do
          program_ptr <- peekWordOff fleet_ptr (fleetProgramOf index)
          tombstones_ptr <- peekWordOff program_ptr programInputError
          fact_count <- peekWordOff program_ptr programInputNewCount
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
            slice dst fact_index =
              forM_ [0 .. struct_count - 1] $ \field_index -> do
                ptr_head <- peekWordOff program_ptr (input_start + field_index)
                let ptr_src = plusPtr ptr_head (fact_index  * 8)
                    ptr_dst = plusPtr dst      (field_index * 8)
                copyBytes ptr_dst ptr_src 8

            peekInputs xs fact_index
              | fact_index == fact_count = return xs
              | otherwise = do
                  tombstone <- liftIO $ peekWordOff tombstones_ptr fact_index

                  x <- case errorOfWord tombstone of
                     ExceptNotAnError -> do
                       liftIO $ slice buf fact_index
                       VRight . snd <$> peekOutput buf 0 ty
                     e ->
                       pure (VLeft (VError e))

                  peekInputs (xs <> [x]) (fact_index + 1)

          (attribute,) <$> peekInputs [] 0
        return x

--------------------------------------------------------------------------------

data TestError
  = ZebraError Striped.StripedError
  | ImpossibleValue ValType [BaseValue]
  | ImpossibleTopValue ValType [BaseValue]
  | ImpossibleFacts ValType [BaseValue]
  deriving (Show)

data ZebraWellTyped = ZebraWellTyped {
    zWellTyped :: WellTyped
  , zFacts     :: Map Zebra.Entity [((Attribute, ValType), [BaseValue])]
  -- ^ Facts grouped by entity attribute, for checking against values translated from zebra.
  }

instance Show ZebraWellTyped where
  show = show . PP.pretty

instance PP.Pretty ZebraWellTyped where
  pretty z =
    PP.vsep
      [ "As Zebra entities ="
      , PP.indent 2 . PP.vsep . fmap (PP.text . ppShow) . Map.keys . zFacts $ z
      , "Expected values (in this order) ="
      , PP.indent 2 . PP.vsep . fmap PP.pretty . List.concat . List.concat . fmap (fmap snd) . Map.elems . zFacts $ z
      ]

-- We don't read arrays (except of bytes -- which are icicle strings) in zebra
jInputType :: Jack InputType
jInputType = do
  let
    zebraSupported t =
      case unInputType t of
        SumT _ (ArrayT _) ->
          False
        _ ->
          True
  arbitrary `suchThat` zebraSupported

jZebraWellTyped :: Jack ZebraWellTyped
jZebraWellTyped = justOf $ do
  wt <- arbitrary
  zebraOfWellTyped wt

gWellTyped :: InputType -> Gen WellTyped
gWellTyped ty = do
  !wt <- tryGenWellTypedWithInput AllowDupTime ty
  case wt of
    Nothing ->
      discard
    Just w ->
      return w

zebraOfWellTyped :: WellTyped -> Jack ZebraWellTyped
zebraOfWellTyped welltyped = do
  let
    -- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
    ignoreTime eavt =
      eavt { eavtValue = AsAt (atFact (eavtValue eavt)) (Icicle.timeOfDays 0) }
    wt =
      welltyped { wtFacts = fmap ignoreTime (wtFacts welltyped) }
    -- Need to include all attributes even if they have no facts.
    -- /O(python)/
    groupedByEntityAttribute =
      Map.fromList $
        flip fmap (List.nub . fmap eavtEntity . wtFacts $ wt) $ \entity ->
          (entity,) $ flip fmap (wtAttributes wt) $ \attribute ->
              let
                a = wtAttribute attribute
              in
                ((a, wtFactType attribute),
                  fmap (atFact . eavtValue) .
                  List.filter (\w -> eavtEntity w == entity && eavtAttribute w == a) $
                    wtFacts wt)

  xs <-
    forM (Map.toList groupedByEntityAttribute) $ \(entity, groupedByAttribute) -> do
      let
        e =
          Zebra.EntityId (Text.encodeUtf8 (getEntity entity))
      zAttributes <-
        forM groupedByAttribute $ \((_, ty), values) -> do
          case zebraOfFacts ty values of
            Left err ->
              Savage.error (show err)
            Right (_, tombstones, _, table) -> do
              let
                n =
                  length values
              Zebra.Attribute
                <$> pure (Storable.fromList (List.replicate n 0))
                <*> (Storable.fromList <$> vectorOf n Zebra.jFactsetId)
                <*> pure (Storable.fromList tombstones)
                <*> pure table
      let
        zEntity =
          Zebra.Entity (Zebra.hashEntityId e) e . Boxed.fromList $ zAttributes
      return (zEntity, groupedByAttribute)

  return . ZebraWellTyped wt . Map.fromList $ xs

schemaOfType :: ValType -> Schema.Column
schemaOfType ty = case ty of
  BoolT ->
    Schema.bool Schema.DenyDefault

  TimeT ->
    Schema.Int Schema.DenyDefault Zebra.TimeSeconds

  DoubleT ->
    Schema.Double Schema.DenyDefault

  IntT ->
    Schema.Int Schema.DenyDefault Zebra.Int

  StringT ->
    Schema.Nested $ Schema.Binary Schema.DenyDefault Zebra.Utf8

  ErrorT ->
    Schema.Int Schema.DenyDefault Zebra.Int

  UnitT ->
    Schema.Unit

  FactIdentifierT ->
    Schema.Int Schema.DenyDefault Zebra.Int

  ArrayT t ->
    Schema.Nested . Schema.Array Schema.DenyDefault $ schemaOfType t

  BufT _ t ->
    Schema.Nested . Schema.Array Schema.DenyDefault $ schemaOfType t

  PairT a b -> do
    let
      a' = schemaOfType a
      b' = schemaOfType b
    in
      Schema.Struct $ Cons.from2
        (Schema.Field (Schema.FieldName "fst") a')
        (Schema.Field (Schema.FieldName "snd") b')

  OptionT t ->
    Schema.option Schema.DenyDefault . schemaOfType $ t

  SumT a b ->
    let
      leftOf =
        Schema.Variant (Schema.VariantName "left") . schemaOfType
      rightOf =
        Schema.Variant (Schema.VariantName "right") . schemaOfType
      a' =
        leftOf a
      b' =
        rightOf b
    in
      Schema.Enum Schema.DenyDefault $ Cons.from2 a' b'

  MapT k v -> do
    let
      k' =
        schemaOfType k
      v' =
        schemaOfType v
    in
      Schema.Nested $ Schema.Map Schema.DenyDefault k' v'

  StructT struct
    -- structs must have at leaast one field in both zebra and icicle
    | (f:fs) <- Map.toList (getStructType struct) ->
      let
        fieldOf (x, t) =
          Schema.Field (Schema.FieldName (nameOfStructField x)) $ schemaOfType t
        f' =
          fieldOf f
        fs' =
          fmap fieldOf fs
      in
        Schema.Struct Schema.DenyDefault . Cons.fromNonEmpty $ f' :| fs'
    | otherwise ->
        Savage.error "Impossible! Generated an empty struct."

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
    Left (ImpossibleValue ty [val])

  VError e ->
    pure . Logical.Int . fromIntegral . wordOfError $ e

  _ ->
    Left (ImpossibleValue ty [val])

zebraOfTopValue :: ValType -> BaseValue -> Either TestError (Zebra.Tombstone, Logical.Value)
zebraOfTopValue t val
  | VRight v <- val
  = (Zebra.NotTombstone,) <$> zebraOfValue t v

  | VLeft (VError ExceptTombstone) <- val
  = (Zebra.Tombstone,) <$> zebraOfValue t (defaultOfType t)

  | otherwise
  = Left (ImpossibleTopValue t [val])

-- | Convert rows of facts into zebra logical tables.
zebraOfFacts ::
     ValType
  -> [BaseValue]
  -> Either TestError (ValType, [Zebra.Tombstone], [Logical.Value], Striped.Table)
zebraOfFacts t facts = do
  let
    schema = schemaOfType t
  (tombstones, rows) <- List.unzip <$> mapM (zebraOfTopValue t) facts
  rows' <- first ZebraError . Striped.fromValues schema . Boxed.fromList $ rows
  let
    table = Striped.Array Schema.DenyDefault rows'
  pure (t, tombstones, rows, table)

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
          (ZebraConfig (eavlMaxMapSize (wtEvalContext wt)))
          (Snapshot testSnapshotTime)
          (PsvOutputConfig (Snapshot testSnapshotTime) PsvOutputDense defaultOutputMissing))
        (InputOpts AllowDupTime Map.empty)
        ("" :: String)
    attrs = fmap (\w -> (wtInputId w, wtAvalancheFlat w :| [])) (wtAttributes wt)

  src <- codeOfPrograms "Icicle.Test.Sea.Zebra.codeOf" dummy (fmap fst attrs) attrs

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

