{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Icicle.Test.Sea.Zebra where

import           Control.Monad.Catch (bracket)
import           Control.Monad.IO.Class (liftIO)

import           Data.String
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Text.Show.Pretty (ppShow)

import           Foreign
import           Foreign.C.String

import           System.IO

import           P

import           X.Control.Monad.Trans.Either (hoistEither, firstEitherT, bracketEitherT')

import           Disorder.Core.IO (testIO)
import           Disorder.Jack (Property, Jack)
import           Disorder.Jack (gamble, arbitrary, (===), justOf, vectorOf)

import           Jetski

import qualified Anemone.Foreign.Mempool as Mempool
import           Anemone.Foreign.Segv (withSegv)

import qualified Test.Zebra.Jack as Zebra
import qualified Zebra.Foreign.Entity as Zebra
import qualified Zebra.Data.Entity as Zebra
import qualified Zebra.Data.Table as Zebra
import qualified Zebra.Data.Core as Zebra

import qualified Icicle.Internal.Pretty as PP
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Data
import qualified Icicle.Data.Time as Icicle
import           Icicle.Sea.FromAvalanche.Base (seaOfTime)
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
  gamble zebra $ \(ZebraWellTyped wt ty facts entity) ->
  testIO . withSegv (pp wt entity) . bracket Mempool.create Mempool.free $ \pool -> do
    c_entity <- Zebra.foreignOfEntity pool entity
    Test.runRight $ do
      code <- hoistEither $ codeOf wt
      src <- firstEitherT SeaJetskiError $ readLibrary code
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

             _ <- liftIO . withCStringLen (ByteString.unpack . Zebra.unEntityId . Zebra.entityId $ entity)
               $ \(id_bytes, id_length) -> do
                 _ <- test_fleet [ argPtr id_bytes, argInt32 (fromIntegral id_length), argPtr nullPtr, argPtr fleet_ptr ]
                 _ <- test_read_entity [ argPtr state, argPtr (Zebra.unCEntity c_entity) ]
                 return ()

             -- if no fact was read, we don't want to peek the input struct
             if length facts > 0
             then do
               programs0_ptr <- peekWordOff fleet_ptr 4
               -- iprogram: { mempool, input, ... }
               -- input: { chord_time, fact_count, tombstone, input_start, ... }
               input_start <- peekWordOff programs0_ptr 4

               let
                 peekInputs xs 0 _ =
                   return xs
                 peekInputs xs n offset = do
                   (offset', xs') <- peekOutputs input_start offset [ty]
                   peekInputs (xs <> xs') (n - 1) offset'

               inputs <- peekInputs [] (length facts) 0
               return $ facts === inputs
             else
               return $ length facts === 0
          )

--------------------------------------------------------------------------------

data ZebraWellTyped = ZebraWellTyped {
    zWelltyped  :: WellTyped
  , zFactType   :: ValType -- wtFactType = Sum Error FactType
  , zFacts      :: [BaseValue] -- wtFacts = zFacts + tombstones
  , zEntity     :: Zebra.Entity ()
  }

instance Show ZebraWellTyped where
  show (ZebraWellTyped wt _ _ e) =
    pp wt e

zebra :: Jack ZebraWellTyped
zebra = justOf (zebraOfWellTyped =<< arbitrary)

zebraOfWellTyped :: WellTyped -> Jack (Maybe ZebraWellTyped)
zebraOfWellTyped wt =
  case zebraOfFacts (wtFactType wt) (wtFacts wt) of
    Nothing ->
      return Nothing
    Just (ty, tombstones, facts, table) -> do
      -- FIXME ignoring fact times for now, but to test it we should convert icicle time to 1600 epoch secs here
      -- let ts = fmap (Zebra.Time . fromIntegral . Icicle.secondsCountJulian . atTime) (wtFacts wt)
      let ts = List.replicate (length (wtFacts wt)) 0
      ps <- vectorOf (length ts) Zebra.jFactsetId
      let attribute = Zebra.Attribute (Storable.fromList ts) (Storable.fromList ps) (Storable.fromList tombstones) table
      entity <- uncurry Zebra.Entity <$> Zebra.jEntityHashId <*> pure (Boxed.singleton attribute)
      pure (Just (ZebraWellTyped wt ty facts entity))

zebraOfFacts :: ValType -> [AsAt BaseValue] -> Maybe (ValType, [Zebra.Tombstone], [BaseValue], Zebra.Table ())
--zebraOfFacts x y | trace ("zebra_of_facts: type = " <> show x <> ", facts = " <> show y) False = undefined
zebraOfFacts typ facts = do
  let
    fromVBool val =
      case val of
        VBool True ->
          pure 1
        VBool False ->
          pure 0
        _ ->
          Nothing

    fromVTime val =
      case val of
        VTime t ->
          pure (fromIntegral (Icicle.packedOfTime t))
        _ ->
          Nothing

    fromVInt val =
      case val of
        VInt x ->
          pure (fromIntegral x)
        _ ->
          Nothing

    fromVDouble val =
      case val of
        VDouble x ->
          pure x
        _ ->
          Nothing

    fromVString val =
      case val of
        VString x ->
          pure (ByteString.pack (Text.unpack x))
        _ ->
          Nothing

    fromVUnit val =
      case val of
        VUnit ->
          pure 0
        _ ->
          Nothing

    --fromVArray f val =
    --  case val of
    --    VArray vs ->
    --      f vs
    --    _ ->
    --      Nothing

    fromVOption f val =
      case val of
        VNone ->
          (0,) <$> f []
        VSome v ->
          (1,) <$> f [v]
        _ ->
          Nothing

    fromVSumError f val =
      case val of
        VLeft (VError e) ->
          (fromIntegral (wordOfError e),) <$> f []
        VRight v ->
          (fromIntegral (wordOfError ExceptNotAnError),) <$> f [v]
        _ ->
          Nothing

    fromVSum f g val =
      case val of
        VLeft v ->
          (0,,) <$> f [v] <*> g []
        VRight v ->
          (1,,) <$> f [] <*> g [v]
        _ ->
          Nothing

    fromVError val =
      case val of
        VError _ ->
          pure 1
        _ ->
          Nothing

    mergeValues t vs
      | null vs
      = tableOf t []
      | otherwise
      = pure (concat vs)

    tableOf :: ValType -> [BaseValue] -> Maybe [Zebra.Column ()]
    tableOf ty rows =
      case ty of
        BoolT ->
          pure . Zebra.IntColumn . Storable.fromList <$> mapM fromVBool rows

        TimeT ->
          pure . Zebra.IntColumn . Storable.fromList <$> mapM fromVTime rows

        DoubleT ->
          pure . Zebra.DoubleColumn . Storable.fromList <$> mapM fromVDouble rows

        IntT ->
          pure . Zebra.IntColumn . Storable.fromList <$> mapM fromVInt rows

        UnitT ->
          pure . Zebra.IntColumn . Storable.fromList <$> mapM fromVUnit rows

        ErrorT ->
          pure . Zebra.IntColumn . Storable.fromList <$> mapM fromVError rows

        StringT -> do
          strings <- mapM fromVString rows

          let
            string =
              ByteString.concat strings
            len =
              ByteString.length string
            lens =
              fmap (fromIntegral . ByteString.length) strings

          pure $ [ Zebra.ArrayColumn
                     (Storable.fromList lens)
                     (Zebra.Table () len (Boxed.fromList [ Zebra.ByteColumn string ]))
                 ]

        -- FIXME: we aren't reading nested arrays right now (savages)
        --ArrayT t -> do
        --  vs <- concat <$> mapM (fromVArray (tableOf t)) rows
        --  lens <- mapM (fromVArray (pure . fromIntegral . length)) rows
        --  pure [ Zebra.ArrayColumn
        --           (Storable.fromList lens)
        --           (Zebra.Table () (length vs) (Boxed.fromList vs))
        --       ]

        OptionT t -> do
          (bools, vs) <- List.unzip <$> mapM (fromVOption (tableOf t)) rows
          cols <- mergeValues t vs
          pure $ [ Zebra.IntColumn (Storable.fromList bools) ] <> cols

        SumT ErrorT t -> do
          (bools, vs) <- List.unzip <$> mapM (fromVSumError (tableOf t)) rows
          pure $ [ Zebra.IntColumn (Storable.fromList bools) ] <> concat vs

        SumT l r -> do
          (bools, ls, rs) <- List.unzip3 <$> mapM (fromVSum (tableOf l) (tableOf r)) rows
          lcols <- mergeValues l ls
          rcols <- mergeValues r rs
          pure $ [ Zebra.IntColumn (Storable.fromList bools) ] <> lcols <> rcols

        StructT fields -> do
          let
            structFieldsOf v =
              case v of
                VStruct x
                  -> pure x
                _ ->
                  Nothing
            types =
              getStructType fields
          vs <- fmap (Map.unionsWith (<>) . fmap (fmap pure)) $ mapM structFieldsOf rows
          concat <$> mapM (uncurry tableOf) (Map.elems (Map.intersectionWith (,) types vs))

        _ ->
          Nothing

    valuesOf (SumT ErrorT t) vals = do
      (ts, vs) <- List.unzip <$> mapM (valueOf (defaultOfType t)) vals
      pure (t, ts, vs)
    valuesOf _ _ =
      Nothing

    valueOf def val =
      case val of
        VLeft (VError ExceptTombstone) ->
          pure (Zebra.Tombstone, def)
        VRight v ->
          pure (Zebra.NotTombstone, v)
        _ ->
          Nothing

  (ty, tombstones, values) <- valuesOf typ (fmap atFact facts)
  table <- Zebra.Table () (length values) . Boxed.fromList <$> tableOf ty values
  pure (ty, tombstones, values, table)

testSnapshotTime :: Time
testSnapshotTime = Icicle.unsafeTimeOfYMD 9999 1 1

codeOf :: WellTyped -> Either SeaError SourceCode
codeOf wt = do
  let
    dummy = HasInput
      (FormatZebra (Snapshot testSnapshotTime) (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing))
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


pp :: WellTyped -> Zebra.Entity () -> String
pp wt entity =
  "Fact type = " <> show (wtFactType wt) <> "\n" <>
  "Facts = " <> ppShow (wtFacts wt) <> "\n" <>
  "As zebra entity = \n" <> ppShow entity

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunNormal checkArgs
