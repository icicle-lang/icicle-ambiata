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
  gamble (jZebraChunkSize) $ \zebraChunkSize ->
  gamble (justOf zebra) $ \(ZebraWellTyped wt ty entity rows) ->
  testIO . withSegv (pp wt entity rows zebraChunkSize) . bracket Mempool.create Mempool.free $ \pool -> do
    c_entity <- Zebra.foreignOfEntity pool entity
    Test.runRight $ do
      code <- hoistEither $ codeOf zebraChunkSize wt
      opts <- getCompilerOptions
      bracketEitherT'
        (firstEitherT SeaJetskiError $ compileLibrary NoCacheLibrary opts code)
        (firstEitherT SeaJetskiError . releaseLibrary)
        (\src -> do
          init <- firstEitherT SeaJetskiError $ function src "zebra_alloc_state" (retPtr retVoid)
          end <- firstEitherT SeaJetskiError $ function src "zebra_collect_state" (retPtr retVoid)
          test_read_entity <- firstEitherT SeaJetskiError $ function src "test_zebra_read_entity" (retPtr retWord8)

          withWords 7 $ \config -> do
            pokeWordOff config 6 defaultPsvOutputBufferSize
            bracketEitherT'
              (liftIO (init [ argPtr nullPtr, argPtr config, argInt64 1 ]))
              (\state -> (liftIO (end [ argPtr config, argPtr state ])))
              (\state -> do
                 fleet_ptr <- peekWordOff state 5

                 liftIO $ do
                   e <- test_read_entity
                           [ argPtr nullPtr
                           , argPtr state
                           , argPtr (Zebra.unCEntity c_entity) ]

                   when (e /= nullPtr) $ do
                     err <- peekCString (castPtr e)
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
  , zRows         :: [Zebra.Value]
  }

instance Show ZebraWellTyped where
  show (ZebraWellTyped wt _ e rs) =
    pp wt e rs 0

jZebraChunkSize :: Jack Int
jZebraChunkSize =
  arbitrary `suchThat` (> 0)

zebra :: Jack (Maybe ZebraWellTyped)
zebra = do
  -- we don't read arrays (except of bytes) in zebra
  let
    supportedInputType x =
      case wtFactType x of
        SumT _ (ArrayT _) -> False
        _ -> True
  wt <- arbitrary `suchThat` supportedInputType
  zebraOfWellTyped wt

zebraOfWellTyped :: WellTyped -> Jack (Maybe ZebraWellTyped)
zebraOfWellTyped wt =
  case zebraOfFacts (wtFactType wt) (fmap atFact (wtFacts wt)) of
    Left e ->
      Savage.error (show e)
    Right Nothing ->
      return Nothing
    Right (Just (ty, tombstones, rows, table)) -> do
      -- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
      -- let ts = fmap (Zebra.Time . fromIntegral . Icicle.secondsCountJulian . atTime) (wtFacts wt)
      let ts = List.replicate (length (wtFacts wt)) 0
      ps <- vectorOf (length ts) Zebra.jFactsetId
      let attribute = Zebra.Attribute (Storable.fromList ts) (Storable.fromList ps) (Storable.fromList tombstones) table
      entity <- uncurry Zebra.Entity <$> Zebra.jEntityHashId <*> pure (Boxed.singleton attribute)
      pure . Just $ ZebraWellTyped wt ty entity rows

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

testSnapshotTime :: Time
testSnapshotTime = Icicle.unsafeTimeOfYMD 9999 1 1

codeOf :: Int -> WellTyped -> Either SeaError SourceCode
codeOf zebra_chunk_size wt = do
  let
    input =
      HasInput
        (FormatZebra
          (ZebraConfig zebra_chunk_size)
          (Snapshot testSnapshotTime)
          (PsvOutputConfig (Snapshot testSnapshotTime) PsvOutputDense defaultOutputMissing))
        (InputOpts AllowDupTime Map.empty)
        ("" :: String)
    attr = wtAttribute wt
    flat = wtAvalancheFlat wt

  src <- codeOfPrograms input [attr] [(attr, flat)]

  pure . textOfDoc . PP.vsep $
    [ PP.pretty src
    , ""
    , "ierror_msg_t test_zebra_read_entity (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity) {"
    , "    return zebra_read_step (piano, state, entity);"
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
    ]


pp :: WellTyped -> Zebra.Entity Schema -> [Zebra.Value] -> Int -> String
pp wt entity rows size =
  "=== Entity ===\n" <>
  "Zebra chunk size = " <> show size <> "\n" <>
  "Fact type = " <> show (wtFactType wt) <> "\n" <>
  "Facts = " <> ppShow (wtFacts wt) <> "\n" <>
  "As Zebra values = \n" <> ppShow rows <> "\n" <>
  "As Zebra entity = \n" <> ppShow entity
  "Avalanche program = \n" <> show (PP.pretty (wtAvalancheFlat wt))

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
