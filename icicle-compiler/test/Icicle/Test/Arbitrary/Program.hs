{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Program where

import           Icicle.BubbleGum (BubbleGumFact(..), Flavour(..))
import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Data
import           Icicle.Common.Eval
import           Icicle.Common.Type
import           Icicle.Data
import           Icicle.Data.Time
import           Icicle.Encoding (renderValue, renderOutputValue)
import           Icicle.Internal.Pretty
import           Icicle.Sea.Eval
import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary.Core
import           Icicle.Test.Arbitrary.Data
import qualified Icicle.Avalanche.Annot as A
import qualified Icicle.Avalanche.Check as A
import qualified Icicle.Avalanche.FromCore as A
import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Avalanche.Simp as A
import qualified Icicle.Avalanche.Statement.Flatten as A
import qualified Icicle.Core.Eval.Program   as Core
import qualified Icicle.Core.Exp.Prim      as Core
import qualified Icicle.Core.Program.Check as Core
import qualified Icicle.Core.Program.Program as Core
import qualified Icicle.Sea.Eval as Sea
import qualified Icicle.Sea.FromAvalanche.Analysis as Sea
import qualified Icicle.Sea.FromAvalanche.State as Sea
import qualified Icicle.Sea.Data as Sea

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import           Data.Maybe
import           Data.String
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.List.NonEmpty as NonEmpty

import           Text.Show.Pretty (ppShow)

import           P

import qualified Prelude as Savage

import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.IO
import           System.IO.Temp (createTempDirectory)

import           Disorder.Corpus

import           Test.QuickCheck (Gen, Arbitrary(..), elements, getPositive, discard)
import           Test.QuickCheck (listOf, listOf1)

import           X.Control.Monad.Trans.Either (EitherT, bracketEitherT')


newtype InputType = InputType {
    unInputType :: ValType
  } deriving (Show)

data WellTyped = WellTyped {
    wtEvalContext :: EvalContext
  , wtFacts       :: [WellTypedValue]
  , wtClusters    :: [WellTypedCluster]
  }

data WellTypedValue = WellTypedValue {
    eavtEntity  :: !Entity
  , eavtInputId :: !InputId
  , eavtValue   :: !(AsAt BaseValue)
  }

-- FIXME
--
-- This only gnerates one compute kernel for each cluster.
-- PsvFission tests multiple kernels by doing its own thing.
-- Perhaps roll that into this as well.
--
data WellTypedCluster = WellTypedCluster {
    wtCluster       :: Sea.Cluster
  , wtFactType      :: !ValType -- clusterInputType == SumT ErrorT wtFactType
  , wtOutputs       :: [(OutputId, ValType)]
  , wtCore          :: !(Core.Program () Var)
  , wtAvalanche     :: !(A.Program () Var Core.Prim)
  , wtAvalancheFlat :: !(A.Program (Annot ()) Var A.Prim)
  }

instance Show WellTyped where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedValue where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedCluster where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Pretty WellTyped where
  pretty wt =
    vsep $
      [ "Well-typed:"
      , indent 2 $ "Attributes ="
      , indent 4 $ vsep (fmap pretty (wtClusters wt))
      , indent 2 $ "Chord time  = " <> (text . show . renderTime TimeSerialisationOutput . evalSnapshotTime . wtEvalContext $ wt)
      , indent 2 $ "Max map size = " <> (text . show . evalMaxMapSize . wtEvalContext $ wt)
      , indent 2 $ "EAVTs ="
      , indent 4 $ vsep (fmap pretty (wtFacts wt))
      ]

instance Pretty WellTypedValue where
  pretty eavt =
    encloseSep lbracket rbracket comma $
      [ pretty (eavtEntity eavt)
      , pretty (eavtInputId eavt)
      , text . show . atFact . eavtValue $ eavt
      , text . show . renderTime TimeSerialisationInput . atTime . eavtValue $ eavt
      ]

instance Pretty WellTypedCluster where
  pretty wt =
    vsep $
      [ "Cluster input Id = " <> pretty (Sea.clusterInputId . wtCluster $ wt)
      , "Cluster fact type = " <> pretty (wtFactType wt)
      , "Core ="
      , indent 2 $ pretty (wtCore wt)
      --, "Avalanche ="
      --, indent 2 $ pretty (wtAvalanche wt)
      --, "Flat ="
      --, indent 2 $ pretty (wtAvalancheFlat wt)
      ]

instance Arbitrary InputType where
  arbitrary =
    inputTypeOf <$>
      genSupportedFactType

instance Arbitrary WellTyped where
  arbitrary =
    validated 10 (tryGenWellTypedWithInput Sea.DoNotAllowDupTime =<< arbitrary)

instance Arbitrary WellTypedCluster where
  arbitrary =
    validated 10 (tryGenAttributeWithInput =<< arbitrary)

takeFlatProgram :: WellTypedCluster -> (InputId, A.Program (Annot ()) Var A.Prim)
takeFlatProgram cluster =
  (Sea.clusterInputId . wtCluster $ cluster, wtAvalancheFlat cluster)

-- | Savagely big (hopefully enough) max map size
genSufficientMaxMapSize :: Int -> Gen Int
genSufficientMaxMapSize numFacts = do
  n <- getPositive <$> arbitrary
  return (n * numFacts)

tryGenWellTypedForSingleAttribute ::
     Sea.InputAllowDupTime
  -> WellTypedCluster
  -> Gen (Maybe WellTyped)
tryGenWellTypedForSingleAttribute allowDupTime attribute = do
  (facts, time) <- genFactsForAttributes allowDupTime [attribute]
  maxMapSize <- genSufficientMaxMapSize (length facts)
  return . Just . WellTyped (EvalContext time maxMapSize) facts $ [attribute]

tryGenWellTypedWithInput ::
     Sea.InputAllowDupTime
  -> InputType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInput allowDupTime ty = do
  as <-
    replaceInputNamesWithUniques <$>
      validatedNonEmpty 10
        (tryGenAttributeWithInput ty)
  (facts, time) <- genFactsForAttributes allowDupTime as
  maxMapSize <- genSufficientMaxMapSize (length facts)
  return . Just . WellTyped (EvalContext time maxMapSize) facts $ as

tryGenWellTypedWithInputAndOutput ::
     Sea.InputAllowDupTime
  -> InputType
  -> ValType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInputAndOutput allowDupTime inputType outputType = do
  as <-
    replaceInputNamesWithUniques <$>
      validatedNonEmpty 10
        (tryGenAttributeWithInputAndOutput inputType outputType)
  (facts, time) <- genFactsForAttributes allowDupTime as
  maxMapSize <- genSufficientMaxMapSize (length facts)
  return . Just . WellTyped (EvalContext time maxMapSize) facts $ as

tryGenWellTypedFromCore' ::
     Sea.InputAllowDupTime
  -> InputType
  -> Core.Program () Var
  -> Gen (Either Savage.String WellTyped)
tryGenWellTypedFromCore' allowDupTime ty core = do
  attrs <-
    fmap replaceInputNamesWithUniques <$>
      validatedNonEmpty' 10
        (tryGenAttributeFromCore' ty core)
  case attrs of
    Left e ->
      return (Left e)
    Right as -> do
      (facts, time) <- genFactsForAttributes allowDupTime as
      maxMapSize <- genSufficientMaxMapSize (length facts)
      return . Right . WellTyped (EvalContext time maxMapSize) facts $ as

replaceInputNamesWithUniques :: [WellTypedCluster] -> [WellTypedCluster]
replaceInputNamesWithUniques =
  flip List.zipWith [(0::Int)..] $ \i w ->
    w
      { wtCluster =
          (wtCluster w)
            { Sea.clusterInputId
                = fromJust (parseInputId ("test:input_" <> Text.pack (show i)))
            }
       }

genWellTypedWithDuplicateTimes :: Gen WellTyped
genWellTypedWithDuplicateTimes
 = validated 10 (tryGenWellTypedWithInput Sea.AllowDupTime =<< arbitrary)

evalWellTyped :: WellTyped -> Int -> Map Entity [(OutputId, BaseValue)]
evalWellTyped wt factsLimit =
    Map.unionsWith (<>) .
    fmap (evalWellTypedCluster factsLimit (wtEvalContext wt) (wtFacts wt)) .
    wtClusters $
      wt

-- | NOTE
--   This duals as a specification for PSV dropping semantics.
--
evalWellTypedCluster ::
     Int
  -> EvalContext
  -> [WellTypedValue]
  -> WellTypedCluster
  -> Map Entity [(OutputId, BaseValue)]
evalWellTypedCluster factsLimit evalContext wellTypedValues wtc =
  let
    cluster =
      wtCluster wtc
    inputId =
      Sea.clusterInputId cluster
    factType =
      wtFactType wtc
    core =
      wtCore wtc

    -- Check if the entity has any attribute with a fact count exceeding the facts limit.
    -- If so drop the entity (i.e. this entity will not be in the psv sparse or dense output)
    -- This needs to be done before we add the dummy entities so that entities that exceed
    -- the limnit still get an output if the query returns a constant.
    entitiesExceedingLimit =
      List.nub .
      fmap fst .
      Map.keys .
      Map.filter (\c -> c > factsLimit) .
      foldr (\v m -> Map.insertWith (+) (eavtEntity v, eavtInputId v) 1 m) Map.empty $
        wellTypedValues

    -- If there is no fact or if all facts are tombstones for this attribute, we still
    -- want to run compute.
    -- e.g. input: homer|tombstone|2000-1-1
    --      query: feature foo  ~> None
    --      should still gives:
    --      homer|None
    --      marge|None
    factsNotForThisInputId v =
      eavtInputId v /= inputId ||
      atFact (eavtValue v) == VError ExceptTombstone

    entitiesWithNoFactsOrAllTombstones =
      List.nub .
      fmap eavtEntity .
      List.concat .
      List.filter (not . null) .
      List.groupBy ((==) `on` eavtEntity).
      List.filter factsNotForThisInputId $
        wellTypedValues

    entitiesThatWillNotBeComputed =
      entitiesExceedingLimit <> entitiesWithNoFactsOrAllTombstones

    dummyValue =
      AsAt
        (BubbleGumFact (Flavour 0 (unsafeTimeOfYMD 0 0 0)), defaultOfType factType)
        (unsafeTimeOfYMD 0 0 0)

    dummyEntities =
      fmap ((inputId,) . (, dummyValue)) entitiesThatWillNotBeComputed

    -- Only run compute on the facts for this attribute.
    mkComputeInput ix (WellTypedValue entity a (AsAt fact time)) =
      (a, (entity, AsAt (BubbleGumFact (Flavour ix time), fact) time))

    actualInputs =
      List.zipWith mkComputeInput [0..] .
      List.filter (not . (`elem` entitiesThatWillNotBeComputed) . eavtEntity) $
        wellTypedValues

    inputs =
      List.groupBy ((==) `on` fst) .
      fmap snd $
        actualInputs <> dummyEntities

    eval values =
      case Core.eval evalContext values core of
        Left e ->
          Savage.error ("Impossible! Failed to evaluate well-typed Core: " <> show e)
        Right xs ->
          xs

    groupByEntity xs =
      case xs of
        [] ->
          []
        (e,x):xss ->
          [(e,x:fmap snd xss)]
  in
    Map.fromList .
    fmap (second (Core.value . eval)) .
    concatMap groupByEntity $
      inputs


--------------------------------------------------------------------------------

genVTs ::
     Sea.InputAllowDupTime
  -> InputType
  -> Gen [AsAt BaseValue]
genVTs allowDupTime (unInputType -> ty) = do
  (inputs, _) <-
    case allowDupTime of
      Sea.AllowDupTime ->
        inputsForType ty
      Sea.DoNotAllowDupTime ->
        first (List.sortBy (compare `on` atTime) . List.nubBy ((==) `on` atTime)) <$> inputsForType ty
  let
    valueOrTombstone (VLeft (VError _)) =
      VLeft (VError ExceptTombstone)
    valueOrTombstone v =
      v
  return (fmap (fmap (valueOrTombstone . snd)) $ inputs)

genEAVTs ::
     Sea.InputAllowDupTime
  -> [(InputId, InputType)]
  -> Gen [WellTypedValue]
genEAVTs allowDupTime tys = do
  e <- Entity <$> elements southpark
  let
    entities =
      [e]
  vs <-
    forM entities $ \entity -> do
      forM tys $ \(attr, ty) -> do
        vs <- genVTs allowDupTime ty
        return . fmap (WellTypedValue entity attr) $ vs

  return . List.concat . List.concat $ vs

genFactsForAttributes ::
     Sea.InputAllowDupTime
  -> [WellTypedCluster]
  -> Gen ([WellTypedValue], Time)
genFactsForAttributes allowDupTime as = do
  facts <- genEAVTs allowDupTime (fmap takeInput as)
  case facts of
    [] ->
      (facts,) <$> arbitrary
    _ ->
      let
        latestTime =
          List.maximum . fmap (atTime . eavtValue) $ facts
      in
        return (facts, latestTime)

takeInput :: WellTypedCluster -> (InputId, InputType)
takeInput w =
  (Sea.clusterInputId . wtCluster $ w, InputType . Sea.clusterInputType . wtCluster $ w)

--------------------------------------------------------------------------------

inputTypeOf :: ValType -> InputType
inputTypeOf ty
    = InputType $ SumT ErrorT ty

tryGenAttributeWithInput ::
     InputType
  -> Gen (Maybe WellTypedCluster)
tryGenAttributeWithInput ty = do
  core <- programForStreamType (unInputType ty)
  tryGenAttributeFromCore ty core

tryGenAttributeWithInputAndOutput ::
     InputType
  -> ValType
  -> Gen (Maybe WellTypedCluster)
tryGenAttributeWithInputAndOutput i out = do
 wt <- tryGenAttributeWithInput i
 case wt of
  Just wt' -> return $ do
    checked <- fromEither $ Core.checkProgram $ wtCore wt'
    case any ((==out) . functionReturns . snd) $ checked of
     True -> return wt'
     False -> Nothing
  _ -> return Nothing

tryGenAttributeFromCore ::
     InputType
  -> Core.Program () Var
  -> Gen (Maybe WellTypedCluster)
tryGenAttributeFromCore ty core =
   fromEither <$> tryGenAttributeFromCore' ty core

tryGenAttributeFromCore' ::
     InputType
  -> Core.Program () Var
  -> Gen (Either Savage.String WellTypedCluster)
tryGenAttributeFromCore' (InputType sty) core
  | SumT ErrorT ty <- sty = do
      let
        replaceStmts prog stms
          = prog { A.statements = stms }

        namer =
          A.namerText (flip Var 0)

        dummyAnn =
          Annot (FunT [] UnitT) ()

      attribute <- arbitrary

      return $ do
        checked <- nobodyCares (Core.checkProgram core)
        let outputs = fmap (second functionReturns) checked
        _       <- traverse (supportedOutputType . functionReturns . snd) checked
        let avalanche = testFresh "fromCore" $ A.programFromCore namer core
        flatStmts <- nobodyCares (testFreshT "anf" $ A.flatten () (A.statements avalanche))
        flattened <- nobodyCares (A.checkProgram A.flatFragment (replaceStmts avalanche flatStmts))
        unchecked <- nobodyCares (testFresh "simp" $ A.simpFlattened dummyAnn A.defaultSimpOpts flattened)
        simplified <- nobodyCares (A.checkProgram A.flatFragment (A.eraseAnnotP unchecked))

        _ <-
          sequence .
          fmap supportedOutputType .
          Set.toList .
          Sea.typesOfProgram $
            simplified

        -- FIXME
        -- Single-kernel cluster only (see comment above).
        let clusterId = Sea.ClusterId 0 -- nobody cares
        cluster <- nobodyCares (Sea.clusterOfPrograms clusterId attribute (NonEmpty.fromList [simplified]))

        let
          wta = WellTypedCluster {
            wtCluster       = cluster
          , wtFactType      = ty
          , wtOutputs       = outputs
          , wtCore          = core
          , wtAvalanche     = avalanche
          , wtAvalancheFlat = simplified
          }

        return wta

  | otherwise =
      Savage.error "Impossible! Generator given an input type that is not Sum Error."

-- If the input are structs, we can pretend it's a dense value
-- We can't treat other values as a single-field dense struct because the
-- generated programs do not treat them as such.
genWellTypedWithStruct :: Sea.InputAllowDupTime -> Gen WellTyped
genWellTypedWithStruct allowDupTime = validated 10 $ do
  st <- arbitrary :: Gen StructType
  tryGenWellTypedWithInput allowDupTime (inputTypeOf $ StructT st)

------------------------------------------------------------------------

validated :: Int -> Gen (Maybe a) -> Gen a
validated n g
  | n <= 0    = discard
  | otherwise = do
      m <- g
      case m of
        Nothing -> validated (n-1) g
        Just x  -> pure x

validated' :: Int -> Gen (Either Savage.String a) -> Gen (Either Savage.String a)
validated' n g
  | n < 0 = discard
  | n == 0 = g
  | otherwise = do
      m <- g
      case m of
        Left _ -> validated' (n-1) g
        Right x  -> pure (Right x)

fromEither :: Either x a -> Maybe a
fromEither (Left _)  = Nothing
fromEither (Right x) = Just x

nobodyCares :: Show a => Either a b -> Either Savage.String b
nobodyCares = first (ppShow)

validatedNonEmpty :: Int -> Gen (Maybe a) -> Gen [a]
validatedNonEmpty n g = do
  x <- validated n g
  xs <- catMaybes <$> listOf g
  return (x:xs)

validatedNonEmpty' :: Int -> Gen (Either Savage.String a) -> Gen (Either Savage.String [a])
validatedNonEmpty' n g = do
  x <- validated' n g
  case x of
    Left a ->
      return (Left a)
    Right b -> do
      xs <- catMaybes . fmap (either (const Nothing) Just) <$> listOf g
      return (Right (b:xs))

------------------------------------------------------------------------

-- * Specialised WellTyped fact type generators to avoid too many discards.

genSupportedArrayStructFactType :: Gen ValType
genSupportedArrayStructFactType =
  ArrayT . StructT <$> genSupportedStructFactType

genSupportedFactType :: Gen ValType
genSupportedFactType =
  oneof_sized_vals
    [ BoolT
    , IntT
    , DoubleT
    , TimeT
    , StringT
    ]
    [ ArrayT <$> genSupportedArrayElemFactType
    , StructT <$> genSupportedStructFactType
    ]

genSupportedArrayElemFactType :: Gen ValType
genSupportedArrayElemFactType =
  -- other types should have been melted
  oneof_sized_vals
    [ BoolT
    , IntT
    , DoubleT
    , TimeT
    , StringT
    ]
    [ StructT <$> genSupportedStructFactType
    ]

genSupportedStructFactType :: Gen StructType
genSupportedStructFactType =
  StructType . Map.fromList <$>
    (List.zip <$>
       listOf1 arbitrary <*>
       listOf1 genSupportedStructFieldFactType
    )

genSupportedStructFieldFactType :: Gen ValType
genSupportedStructFieldFactType =
  oneof_sized_vals
    [ BoolT
    , IntT
    , DoubleT
    , TimeT
    , StringT
    ]
    [ OptionT <$> genPrimitiveFactType
    , StructT <$> genSupportedStructFactType
    ]

genPrimitiveFactType :: Gen ValType
genPrimitiveFactType =
  oneof_vals
    [ BoolT
    , IntT
    , DoubleT
    , TimeT
    , StringT
    ]

--------------------------------------------------------------------------------

-- * Generated WellTyped programs must have those output types.

supportedOutputType :: ValType -> Either String ValType
supportedOutputType t
  | isSupportedPsvOutputType t =
      Right t
  | otherwise =
      -- trace ("discarding: " <> show t ) $
      Left ("Unsupported output: " <> show t)

isSupportedStructOutputType :: StructType -> Bool
isSupportedStructOutputType =
  all isSupportedPsvOutputType . Map.elems . getStructType

isSupportedPsvOutputType :: ValType -> Bool
isSupportedPsvOutputType = \case
  -- FIXME
  -- These are output types unsupported by PSV.
  -- They will be responsible for most of the discards.
  -- Remove them when we switch to Zebra.
  UnitT ->
    False
  SumT a b ->
    case a of
      ErrorT ->
        isSupportedPsvOutputType b
      _ ->
        False

  StructT s ->
    isSupportedStructOutputType s
  BoolT  ->
    True
  TimeT ->
    True
  DoubleT ->
    True
  IntT ->
    True
  StringT ->
    True
  ErrorT ->
    True
  FactIdentifierT ->
    True
  ArrayT t ->
    isSupportedPsvOutputType t
  MapT k v ->
    isSupportedPsvOutputType k && isSupportedPsvOutputType v
  OptionT t ->
    isSupportedPsvOutputType t
  PairT a b ->
    isSupportedPsvOutputType a && isSupportedPsvOutputType b

  -- FIXME
  -- Nested buffers are not possible because buffers are only
  -- generated by latest. The result of a latest is an Aggregate,
  -- and we can't do another latest on that aggregate value.
  -- It is possible to randomly generate a Core program with nested
  -- however, so we just outlaw it here.
  -- It should be possible to do this better to reduce the discards.
  BufT _ t ->
    isSupportedPsvOutputTypeNoBuf t

isSupportedPsvOutputTypeNoBuf :: ValType -> Bool
isSupportedPsvOutputTypeNoBuf = \case
  BufT{} ->
    False
  t ->
    isSupportedPsvOutputType t

--------------------------------------------------------------------------------

genPsvConstants :: WellTyped -> Gen Sea.PsvConstants
genPsvConstants wt = do
  -- maximum number of rows to read before compute
  let inc x = x + 1
  maxRowCount <- inc . getPositive <$> arbitrary
  -- the buffer needs to be at least as large as a single line
  let str x = x + longestLine wt + 4
  inputBuf <- str . getPositive <$> arbitrary
  let outputBuf = inputBuf
  factsLimit <- inc . getPositive <$> arbitrary
  return $ Sea.PsvConstants maxRowCount inputBuf outputBuf factsLimit (evalMaxMapSize (wtEvalContext wt))

longestLine :: WellTyped -> Int
longestLine wt
  | List.null (wtFacts wt)
  = 0
  | otherwise
  = fromIntegral
  $ LT.length
  $ List.maximumBy (compare `on` LT.length)
  $ fmap (LT.intercalate "|")
  $ fmap fieldsOfFact $ wtFacts wt

textOfOutputs :: Map Entity [(OutputId, BaseValue)] -> LT.Text
textOfOutputs =
  LT.unlines . linesOfOutputs

linesOfOutputs :: Map Entity [(OutputId, BaseValue)] -> [LT.Text]
linesOfOutputs =
  let
    lineOf e (n, v) =
      case textOfOutputValue v of
        Nothing ->
          Nothing
        Just u
          | u == LT.fromStrict tombstone ->
              Nothing
          | otherwise ->
              Just . LT.intercalate "|" $
                [ LT.fromStrict . getEntity $ e
                , LT.fromStrict . renderOutputId $ n
                , u ]
  in
    List.concat . fmap (catMaybes . (\(e, vs) -> fmap (lineOf e) vs)) . Map.toList

textOfOutputValue :: BaseValue -> Maybe LT.Text
textOfOutputValue v
 = do v' <- valueFromCore v
      t  <- renderOutputValue v'
      return $ LT.replace "\n" "\\n" $ LT.fromStrict t

textSubstitution :: LT.Text -> LT.Text
textSubstitution = LT.replace "\n" "\\n"


textOfFacts :: [WellTypedValue] -> LT.Text
textOfFacts vs =
  LT.unlines (fmap (LT.intercalate "|") (fmap fieldsOfFact vs))

fieldsOfFact :: WellTypedValue -> [LT.Text]
fieldsOfFact (WellTypedValue e a v) =
  let
    (valueText, timeText) = textOfInputValues v
  in [ LT.fromStrict (getEntity e), LT.fromStrict (renderInputId a), valueText, timeText ]

textOfInputValues :: AsAt BaseValue -> (LT.Text, LT.Text)
textOfInputValues v =
  (textOfValue (atFact v), textOfInputTime (atTime v))

textOfValue :: BaseValue -> LT.Text
textOfValue
 = LT.replace "\n" "\\n" -- this is the only really special character, not sure how we should deal with this
 . LT.fromStrict
 . renderValue tombstone
 . fromMaybe Tombstone
 . valueFromCore

textOfInputTime :: Time -> LT.Text
textOfInputTime = LT.fromStrict . renderTime TimeSerialisationInput

withSystemTempDirectory :: FilePath -> (FilePath -> EitherT SeaError IO a) -> EitherT SeaError IO a
withSystemTempDirectory template action = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release action


denseTextOfFacts :: [Entity] -> [AsAt BaseValue] -> LT.Text
denseTextOfFacts entities vs =
  LT.unlines (fmap (LT.intercalate "|") (denseFieldsOfFacts entities vs))

denseFieldsOfFacts :: [Entity] -> [AsAt BaseValue] -> [[LT.Text]]
denseFieldsOfFacts entities vs
  | Just (AsAt v t) <- sequence' vs
  , Just fs  <- sequence $ fmap (sequence . flip AsAt t . structValues) v
  =  [ [ LT.fromStrict entity, valueText, timeText ]
     | Entity entity         <- entities
     , (valueText, timeText) <- denseTextOfInputValues fs ]
  | otherwise
  =  [ [ LT.fromStrict entity, valueText, timeText ]
     | Entity entity         <- entities
     , (valueText, timeText) <- fmap textOfInputValues vs ]
  where
    sequence' [] = Nothing
    sequence' (AsAt x t : xs) = Just $ AsAt (x : fmap atFact xs) t
    structValues
      = \case VStruct m -> Just (Map.elems m)
              _         -> Nothing

denseTextOfInputValues :: [AsAt [BaseValue]] -> [(LT.Text, LT.Text)]
denseTextOfInputValues vs =
  List.zip
    (fmap (LT.intercalate "|" . fmap textOfValue . atFact) vs)
    (fmap (textOfInputTime . atTime) vs)

denseDictionary :: OutputId -> ValType -> Gen (Maybe PsvInputDenseDict)
denseDictionary denseName (StructT (StructType m))
  = do missingValue <- genMissingValue
       let n         = renderOutputId denseName
       fs           <- mapM (\(t,v) -> pure . (t,) . (,v) =<< arbitrary)
                            (Map.toList $ Map.mapKeys nameOfStructField m)
       return $ Just
              $ PsvInputDenseDict
                  (Map.singleton (renderOutputId denseName) fs)
                  (maybe Map.empty (Map.singleton n) missingValue)
                  n
denseDictionary _ _ = return Nothing

genMissingValue :: Gen (Maybe Text)
genMissingValue = elements [Nothing, Just "NA", Just ""]

tombstone :: Text
tombstone = "💀"
