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
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Text.Show.Pretty (ppShow)

import           Foreign
import           Foreign.C.String

import           System.IO

import qualified Prelude as Savage 

import           P

import           X.Control.Monad.Trans.Either (hoistEither, firstEitherT, bracketEitherT')

import           Disorder.Core.IO (testIO)
import           Disorder.Jack (Property, Jack)
import           Disorder.Jack (gamble, arbitrary, (===), justOf, vectorOf, suchThat)

import           Jetski

import qualified Anemone.Foreign.Mempool as Mempool
import           Anemone.Foreign.Segv (withSegv)

import qualified Test.Zebra.Jack as Zebra
import qualified Zebra.Foreign.Entity as Zebra
import qualified Zebra.Data.Entity as Zebra
import qualified Zebra.Data.Core as Zebra
import qualified Zebra.Data.Fact as Zebra
import           Zebra.Data.Table (Table(..), TableError)
import qualified Zebra.Data.Table as Table
import           Zebra.Data.Schema (Schema)
import qualified Zebra.Data.Schema as Schema

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
  gamble (justOf zebra) $ \(ZebraWellTyped wt ty entity) ->
  testIO . withSegv (pp wt entity) . bracket Mempool.create Mempool.free $ \pool -> do
    c_entity <- Zebra.foreignOfEntity pool entity
    Test.runRight $ do
      code <- hoistEither $ codeOf wt
      opts <- getCompilerOptions
      bracketEitherT'
        (firstEitherT SeaJetskiError $ compileLibrary NoCacheLibrary opts code)
        (firstEitherT SeaJetskiError . releaseLibrary)
        (\src -> do
          init <- firstEitherT SeaJetskiError $ function src "zebra_alloc_state" (retPtr retVoid)
          end <- firstEitherT SeaJetskiError $ function src "zebra_collect_state" (retPtr retVoid)
          test_fleet <- firstEitherT SeaJetskiError $ function src "test_setup_fleet" (retPtr retVoid)
          test_read_entity <- firstEitherT SeaJetskiError $ function src "test_zebra_read_entity" (retPtr retWord8)

          withWords 7 $ \config -> do
            pokeWordOff config 6 defaultPsvOutputBufferSize
            bracketEitherT'
              (liftIO (init [ argPtr nullPtr, argPtr config ]))
              (\state -> (liftIO (end [ argPtr config, argPtr state ])))
              (\state -> do
                 fleet_ptr <- peekWordOff state 5

                 liftIO . withCStringLen (ByteString.unpack . Zebra.unEntityId . Zebra.entityId $ entity)
                   $ \(id_bytes, id_length) -> do
                     e1 <- test_fleet
                             [ argPtr id_bytes
                             , argInt32 (fromIntegral id_length)
                             , argPtr nullPtr
                             , argPtr fleet_ptr ]

                     when (e1 /= nullPtr) $
                       fail "failed to configure fleet"

                     e2 <- test_read_entity
                             [ argPtr state
                             , argPtr (Zebra.unCEntity c_entity) ]

                     when (e2 /= nullPtr) $ do
                       err <- peekCString (castPtr e2)
                       fail $ "failed to read entity: " <> err

                 -- iprogram: { *mempool, input, ... }
                 -- input: { *chord_time, *fact_count, *tombstone, *input_start, ... }
                 programs0_ptr <- peekWordOff fleet_ptr 4
                 tombstones_ptr <- peekWordOff programs0_ptr 3

                 struct_count <- (\x -> x - 2) . length . stateInputVars <$>
                   hoistEither (stateOfProgram 0 (wtAttribute wt) (wtAvalancheFlat wt))
                 buf <- liftIO $ mallocBytes (struct_count * 8)

                 let
                   input_start
                     = 4

                   facts
                     = fmap atFact (wtFacts wt)

                   nfacts
                     = length facts

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
                       ptr_head <- peekWordOff programs0_ptr (input_start + field_i)
                       let ptr_src = plusPtr ptr_head (fact_i  * 8)
                           ptr_dst = plusPtr dst      (field_i * 8)
                       copyBytes ptr_dst ptr_src 8

                   peekInputs xs 0 =
                     return xs

                   peekInputs xs index = do
                     let fact_i = nfacts - index
                     tombstone <- liftIO $ peekElemOff tombstones_ptr fact_i

                     x <- case errorOfWord tombstone of
                        ExceptNotAnError -> do
                          liftIO $ slice buf fact_i
                          VRight . snd <$> peekOutput buf 0 ty
                        e ->
                          pure (VLeft (VError e))

                     peekInputs (x:xs) (index - 1)

                 inputs <- peekInputs [] nfacts
                 liftIO $ free buf
                 return $ facts === List.reverse inputs
              )
        )

--------------------------------------------------------------------------------

data TestError
  = ZebraError (TableError Schema)
  | UnexpectedError ValType [BaseValue]
  deriving (Show)

data ZebraWellTyped = ZebraWellTyped {
    zWelltyped    :: WellTyped
  , zFactType     :: ValType -- wtFactType = Sum Error FactType
  , zEntity       :: Zebra.Entity Schema
  }

instance Show ZebraWellTyped where
  show (ZebraWellTyped wt _ e) =
    pp wt e

zebra :: Jack (Maybe ZebraWellTyped)
zebra = do
  wt <- arbitrary
  zebraOfWellTyped wt

zebraOfWellTyped :: WellTyped -> Jack (Maybe ZebraWellTyped)
zebraOfWellTyped wt =
  case zebraOfFacts (wtFactType wt) (fmap atFact (wtFacts wt)) of
    Left e ->
      Savage.error (show e)
    Right Nothing ->
      return Nothing
    Right (Just (ty, tombstones, table)) -> do
      -- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
      -- let ts = fmap (Zebra.Time . fromIntegral . Icicle.secondsCountJulian . atTime) (wtFacts wt)
      let ts = List.replicate (length (wtFacts wt)) 0
      ps <- vectorOf (length ts) Zebra.jFactsetId
      let attribute = Zebra.Attribute (Storable.fromList ts) (Storable.fromList ps) (Storable.fromList tombstones) table
      entity <- uncurry Zebra.Entity <$> Zebra.jEntityHashId <*> pure (Boxed.singleton attribute)
      pure . Just $ ZebraWellTyped wt ty entity

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
    pure Schema.Bool

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
    let
      none =
        Schema.Variant (Schema.VariantName "none") (Schema.Struct Boxed.empty)
      someOf x =
        Schema.Variant (Schema.VariantName "some") <$> schemaOfType x
    in Schema.Enum none . Boxed.singleton <$> someOf t

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
    pure . Zebra.Int . fromIntegral $ x

  VDouble x ->
    pure . Zebra.Double $ x

  VUnit ->
    pure . Zebra.Struct $ Boxed.empty

  VBool x ->
    pure . Zebra.Bool $ x

  VTime x ->
    pure . Zebra.Int . fromIntegral . Icicle.packedOfTime $ x

  VString x ->
    pure . Zebra.ByteArray . Text.encodeUtf8 $ x

  VArray xs
    | ArrayT t <- ty
    -> Zebra.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VPair a b
    | PairT ta tb <- ty
    -> do a' <- zebraOfValue ta a
          b' <- zebraOfValue tb b
          pure . Zebra.Struct . Boxed.fromList $ [a', b']

  VLeft x
    | SumT t _ <- ty
    -> Zebra.Enum 0 <$> zebraOfValue t x

  VRight x
    | SumT _ t <- ty
    -> Zebra.Enum 1 <$> zebraOfValue t x

  VNone
    | OptionT _ <- ty
    -> pure . Zebra.Enum 0 . Zebra.Struct $ Boxed.empty

  VSome x
    | OptionT t <- ty
    -> Zebra.Enum 1 <$> zebraOfValue t x

  VMap x
    | MapT tk tv <- ty
    -> do keys <- mapM (zebraOfValue tk) (Map.keys x)
          vals <- mapM (zebraOfValue tv) (Map.elems x)
          pure . Zebra.Struct . Boxed.fromList $
            [ Zebra.Array (Boxed.fromList keys), Zebra.Array (Boxed.fromList vals) ]
  VStruct xs
    | StructT struct <- ty
    , types <- getStructType struct
    , vs <- Map.elems (Map.intersectionWith (,) types xs)
    , length vs == Map.size types
    -> Zebra.Struct . Boxed.fromList <$> mapM (uncurry zebraOfValue) vs
  VBuf xs
    | BufT _ t <- ty
    -> Zebra.Array . Boxed.fromList <$> mapM (zebraOfValue t) xs

  VFactIdentifier x ->
    pure . Zebra.Int . fromIntegral . getFactIdentifierIndex $ x

  VError ExceptTombstone ->
    Left (UnexpectedError ty [val])

  VError e ->
    pure . Zebra.Int . fromIntegral . wordOfError $ e

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
  -> Either TestError (Maybe (ValType, [Zebra.Tombstone], Table Schema))
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
        pure . Just $ (t, tombstones, table)

  | otherwise = Left (UnexpectedError ty facts)

testSnapshotTime :: Time
testSnapshotTime = Icicle.unsafeTimeOfYMD 9999 1 1

codeOf :: WellTyped -> Either SeaError SourceCode
codeOf wt = do
  let
    dummy = HasInput
      (FormatZebra (Snapshot testSnapshotTime)
      (PsvOutputConfig (Snapshot testSnapshotTime) PsvOutputDense defaultOutputMissing))
      (InputOpts AllowDupTime Map.empty)
      ("" :: String)
    attr = wtAttribute wt
    flat = wtAvalancheFlat wt

  src <- codeOfPrograms dummy [attr] [(attr, flat)]

  pure . textOfDoc . PP.vsep $
    [ PP.pretty src
    , ""
    , "ierror_msg_t test_zebra_read_entity (zebra_state_t *state, zebra_entity_t *entity) {"
    , "    return zebra_read_entity (state, entity);"
    , "}"
    , ""
    , "ierror_msg_t test_setup_fleet (const char *entity, size_t size, piano_t *piano, ifleet_t *fleet) {"
    , "    psv_collect_fleet (fleet);"
    , "    return psv_configure_fleet (entity, size, piano, fleet);"
    , "}"
    , ""
    , "static itime_t test_times[] = { " <> seaOfTime testSnapshotTime <> " };"
    , ""
    , "itime_t *test_get_time() {"
    , "    return &test_times;"
    , "}"
    ]


pp :: WellTyped -> Zebra.Entity Schema -> String
pp wt entity =
  "Fact type = " <> show (wtFactType wt) <> "\n" <>
  "Facts = " <> ppShow (wtFacts wt) <> "\n" <>
  "As zebra entity = \n" <> ppShow entity

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
