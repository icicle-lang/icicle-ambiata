{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Program where

import           Icicle.BubbleGum (BubbleGumFact(..), Flavour(..))
import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Data
import           Icicle.Common.Eval
import           Icicle.Common.Type
import           Icicle.Data hiding (StructField)
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
import qualified Icicle.Sea.FromAvalanche.State as Sea
import qualified Icicle.Sea.Data as Sea
import qualified Icicle.Compiler as Compiler

import           Control.Monad.IO.Class (liftIO)
import           Control.Arrow ((&&&))

import           Data.Map (Map)
import           Data.Maybe
import           Data.String
import qualified Data.List as List
import qualified Data.Map as Map
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


-- | inputType = SumT ErrorT factType
newtype SumErrorFactT = SumErrorFactT {
    factType :: ValType
  } deriving (Show)

data WellTyped = WellTyped {
    wtFacts    :: [WellTypedValue]
  , wtClusters :: [WellTypedCluster]
  }

-- FIXME
--
-- This only gnerates one compute kernel for each cluster.
-- PsvFission tests multiple kernels by doing its own thing.
-- Perhaps roll that into this as well.
--
data WellTypedCluster = WellTypedCluster {
    wtCluster       :: Sea.Cluster () ()
  , wtFactType      :: !ValType -- clusterInputType == SumT ErrorT wtFactType
  , wtOutputs       :: [(OutputId, ValType)]
  , wtCore          :: !(Core.Program () Var)
  , wtAvalanche     :: !(A.Program () Var Core.Prim)
  , wtAvalancheFlat :: !(A.Program (Annot ()) Var A.Prim)
  }

data WellTypedValue = WellTypedValue {
    eavtEntity    :: !Entity
  , eavtInputName :: !InputName -- PSV input has no namespace
  , eavtValue     :: !(AsAt BaseValue)
  }

data WellTypedEval = WellTypedEval {
    wtEvalContext :: EvalContext
  , wtEvalFactsLimit :: Int
  }

instance Show WellTyped where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedValue where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedCluster where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedEval where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Pretty WellTyped where
  pretty wt =
    vsep $
      [ "========================================"
      , "Well-typed:"
      , indent 2 $ "EAVTs ="
      , indent 4 $ vsep (fmap pretty (wtFacts wt))
      , ""
      , indent 2 $ "Attributes ="
      , indent 4 $ vsep (fmap pretty (wtClusters wt))
      ]

instance Pretty WellTypedValue where
  pretty eavt =
    encloseSep lbracket rbracket comma $
      [ pretty (eavtEntity eavt)
      , pretty (eavtInputName eavt)
      , text . show . atFact . eavtValue $ eavt
      , text . show . renderTime . atTime . eavtValue $ eavt
      ]

instance Pretty WellTypedCluster where
  pretty wt =
    vsep $
      [ "----------------------------------------"
      , "Cluster input Id = " <> pretty (Sea.clusterInputId . wtCluster $ wt)
      , "Cluster fact type = " <> pretty (wtFactType wt)
      , "Cluster outputs = "
      , indent 2 $ vsep (fmap pretty (Map.toList . fmap Sea.typeLogical . Sea.clusterOutputs . wtCluster $ wt))
      , "Core ="
      , indent 2 $ pretty (wtCore wt)
      --, "Avalanche ="
      --, indent 2 $ pretty (wtAvalanche wt)
      --, "Flat ="
      --, indent 2 $ pretty (wtAvalancheFlat wt)
      ]

instance Pretty WellTypedEval where
  pretty wte =
    vsep $
      [ "----------------------------------------"
      , "Snapshot time = " <> (text . show . evalSnapshotTime . wtEvalContext $ wte)
      , "Max map size = " <> pretty (evalMaxMapSize . wtEvalContext $ wte)
      , "Facts limit = " <> pretty (wtEvalFactsLimit wte)
      ]

--------------------------------------------------------------------------------

asWellTyped :: [WellTypedValue] -> [WellTypedCluster] -> WellTyped
asWellTyped vs =
  WellTyped (sortFacts vs)

-- if an input type is discarded, the output type of the program generated from it
-- is not supported, we should try again.
tryCountInputType :: Int
tryCountInputType = 5

-- if an output type is discarded, it's just not supported.
tryCountOutputType :: Int
tryCountOutputType = 1

-- if a core program is discarded, it must be because the output type is unsupported,
-- or it is not well-typed, so there is no need to try again.
tryCountCoreProgram :: Int
tryCountCoreProgram = 1

genWellTyped :: Gen (Maybe WellTyped)
genWellTyped =
  tryGenWellTypedWithInput Sea.DoNotAllowDupTime =<< (SumErrorFactT <$> arbitrary)

genWellTypedCluster :: Gen (Maybe WellTypedCluster)
genWellTypedCluster =
  tryGenAttributeWithInput =<< (SumErrorFactT <$> arbitrary)

genSumErrorFactType :: Gen SumErrorFactT
genSumErrorFactType =
  SumErrorFactT <$> genFactType

-- FIXME
-- PSV input does not support sum and option types. Remove the restriction when
-- it is gone.
--
genFactType :: Gen ValType
genFactType =
  oneof_sized
    [genNoListFactType] [ArrayT <$> genNoListFactType]

genNoListFactType :: Gen ValType
genNoListFactType =
  oneof_sized
    [ pure BoolT
    , pure DoubleT
    , pure IntT
    , pure TimeT
    , pure StringT
    ]
    [ StructT <$> genStructFactType
    ]

-- FIXME
--
-- Lists as struct fields are disallowed in accordance with Ivory.
-- Nested lists are also disallowed.
-- Chane this if that changes.
--
genStructFactType :: Gen StructType
genStructFactType =
  let
    fieldName i =
      (StructField ("field_" <> Text.pack (show i)),)
  in
    StructType .
    Map.fromList .
    List.zipWith fieldName [(0::Int)..] .
    List.take 100 <$>
      listOf1 genNoListFactType

genWellTypedEvalContext :: Gen WellTypedEval
genWellTypedEvalContext =
  wellTypedEvalContext <$>
    (getPositive <$> arbitrary) <*>
    (getPositive <$> arbitrary)

genWellTypedForSingleAttribute ::
     Sea.InputAllowDupTime
  -> WellTypedCluster
  -> Gen WellTyped
genWellTypedForSingleAttribute allowDupTime attribute = do
  facts <- genEAVTs allowDupTime [attribute]
  return $ asWellTyped facts [attribute]

tryGenWellTypedWithInput ::
     Sea.InputAllowDupTime
  -> SumErrorFactT
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInput allowDupTime ty = do
  attributes <- catMaybes <$> listOf1 (tryGenAttributeWithInput ty)
  if null attributes
  then
   return Nothing
  else do
    let
      clusters =
        replaceInputNamesWithUniques attributes
    facts <- genEAVTs allowDupTime clusters
    return . Just $ asWellTyped facts clusters

tryGenWellTypedWithInputAndOutput ::
     Sea.InputAllowDupTime
  -> SumErrorFactT
  -> ValType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInputAndOutput allowDupTime inputType outputType = do
  attributes <- catMaybes <$> listOf1 (tryGenAttributeWithInputAndOutput inputType outputType)
  if null attributes
  then
   return Nothing
  else do
    let
      clusters =
        replaceInputNamesWithUniques attributes
    facts <- genEAVTs allowDupTime clusters
    return . Just $ asWellTyped facts clusters

tryGenWellTypedFromCore' ::
     Sea.InputAllowDupTime
  -> Core.Program () Var
  -> Gen (Either Savage.String WellTyped)
tryGenWellTypedFromCore' allowDupTime core = do
  attributes <- listOf1 $ tryGenAttributeFromCore' core
  case catMaybes (fmap rightToMaybe attributes) of
    [] ->
      return . fmap (asWellTyped []) . sequence $ attributes
    as -> do
      facts <- genEAVTs allowDupTime as
      return . Right $ asWellTyped facts as

--------------------------------------------------------------------------------

takeFlatProgram :: WellTypedCluster -> (InputId, A.Program (Annot ()) Var A.Prim)
takeFlatProgram cluster =
  (Sea.clusterInputId . wtCluster $ cluster, wtAvalancheFlat cluster)

wellTypedEvalContext :: Int -> Int -> WellTypedEval
wellTypedEvalContext factsLimit maxMapSize =
  let
    upperBoundTimeExclusive =
      unsafeTimeOfYMD 3000 1 1
    ctx =
      EvalContext upperBoundTimeExclusive maxMapSize
  in WellTypedEval ctx factsLimit

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

factsGroupedByInputName :: [WellTypedValue] -> Map InputName (Map Entity [WellTypedValue])
factsGroupedByInputName vs =
  let
    insertByInputName fact =
      Map.insertWith unionByEntity (eavtInputName fact) (Map.singleton (eavtEntity fact) [fact])

    unionByEntity =
      Map.unionWith (<>)
  in
    foldr insertByInputName Map.empty vs

-- | This is used to ensure the Core evaluation semnatics matches the C code.
--
evalWellTyped :: WellTypedEval -> WellTyped -> Map Entity [(OutputId, BaseValue)]
evalWellTyped ctx wt =
  let
    facts =
      wtFacts wt
  in
    Map.unionsWith (<>) .
    flip fmap (wtClusters wt) $ \cluster ->
      evalWellTypedCluster ctx cluster facts

evalWellTypedCluster ::
     WellTypedEval
  -> WellTypedCluster
  -> [WellTypedValue]
  -> Map Entity [(OutputId, BaseValue)]
evalWellTypedCluster eval cluster allFacts =
  let
    evalCore baseValues =
      let
        asStreamValue (AsAt v t) =
          AsAt (BubbleGumFact (Flavour 0 t), v) t
      in
        case Core.eval (wtEvalContext eval) (fmap asStreamValue baseValues) (wtCore cluster) of
          Left e ->
            Savage.error . show . vsep $
              [ "Impossible! Failed to evaluate well-typed Core with this input: "
              , pretty e
              , vsep (fmap (text . show) baseValues)
              ]
          Right xs ->
            xs

    clusterEntities =
      List.nub . fmap eavtEntity $ allFacts

    clusterInputName =
      inputName . Sea.clusterInputId . wtCluster $ cluster

    -- drop all facts for any entity that exceed the limit *for any attribute*

    factsForAllInputs =
      factsGroupedByInputName allFacts

    dropFacts =
      flip fmap factsForAllInputs $ \facts ->
        flip Map.filter facts $ \xs ->
          length xs > wtEvalFactsLimit eval

    dropEntities =
      concatMap Map.keys . Map.elems $ dropFacts

    -- reads facts for this input id that do not exceed the fact limit

    factsForThisInput =
      fromMaybe Map.empty .
      Map.lookup clusterInputName $
        factsForAllInputs

    computeFacts =
      fmap sortFacts .
      Map.filter ((<= wtEvalFactsLimit eval) . length) $
        factsForThisInput

    -- NOTE (invariant)
    -- any entity that was read has an output in psv/zebra
    -- the output handles this logic so we need to emulate that here.

    computeResults =
      fmap (Core.value . evalCore . fmap eavtValue) computeFacts

    defaultOuputsForCluster =
      let
        evalCoreOnNoInput =
          Core.value . evalCore $ []

        replaceRight x =
          case x of
            VRight _ ->
              VLeft (VError ExceptTombstone)
            _ ->
              x
      in
        fmap (second replaceRight) evalCoreOnNoInput

    hasOutput v =
      case v of
        VLeft (VError ExceptNotAnError) ->
          True
        VLeft _ ->
          False
        _ ->
          True

    hasOutputs =
      flip Map.filterWithKey computeResults $ \k v ->
        not (k `elem` dropEntities) &&
        not (null (List.filter (hasOutput . snd) v))

    noOutputs =
      flip List.filter clusterEntities $ \e ->
        not (e `elem` dropEntities) &&
        not (e `elem` Map.keys hasOutputs)

    fillers =
      Map.fromList . fmap (, defaultOuputsForCluster) $ noOutputs

  in
    hasOutputs <> fillers


--------------------------------------------------------------------------------

genVTs :: Sea.InputAllowDupTime -> ValType -> Gen [AsAt BaseValue]
genVTs allowDupTime ft = do
  xs <- inputsForType (SumT ErrorT ft)
  let
    (inputs, _) =
      case allowDupTime of
        Sea.AllowDupTime ->
          xs
        Sea.DoNotAllowDupTime ->
          first (List.nubBy ((==) `on` atTime)) xs

    sorted =
      List.sortBy (compare `on` atTime) inputs

    valueOrTombstone (VLeft _) =
      VLeft . VError $ ExceptTombstone
    valueOrTombstone (VRight a) =
      VRight a
    valueOrTombstone a =
      Savage.error $ "Impossible! Generated an input value that is not a Sum Error: " <> show a

  return . fmap (fmap (valueOrTombstone . snd)) $ sorted

genEs :: Gen [Entity]
genEs =
  let
    icicle =
      [ "amos", "jake", "tran" ]
  in List.sort . fmap Entity <$> listOf1 (elements (southpark <> simpsons <> icicle))

genEAVTs :: Sea.InputAllowDupTime -> [WellTypedCluster] -> Gen [WellTypedValue]
genEAVTs allowDupTime clusters = do
  entities <- genEs
  fmap (
      List.concat .
      -- FIXME
      -- We can't have entities with no facts in sparse PSV, but that
      -- is technically possible in Zebra. Remove this when PSV is gone.
      List.filter (not . null) ) .
    forM entities $ \entity ->
      fmap List.concat .
        forM clusters $ \cluster -> do
          let
            attribute =
              inputName . Sea.clusterInputId . wtCluster $ cluster
            streamFactType =
              wtFactType cluster
          vs <- genVTs allowDupTime streamFactType
          return . fmap (WellTypedValue entity attribute) $ vs

--------------------------------------------------------------------------------

--
-- NOTE
-- May discard because of: output type.
--
tryGenAttributeWithInput :: SumErrorFactT -> Gen (Maybe WellTypedCluster)
tryGenAttributeWithInput ty = do
  core <- programForStreamType . SumT ErrorT . factType $ ty
  let
    simped =
      Compiler.coreSimp core
  tryGenAttributeFromCore simped

--
-- NOTE
-- May discard because of: output type.
--
tryGenAttributeWithInputAndOutput :: SumErrorFactT -> ValType -> Gen (Maybe WellTypedCluster)
tryGenAttributeWithInputAndOutput i out = do
  wta <- tryGenAttributeWithInput i
  case wta of
    Nothing ->
      return Nothing
    Just wt -> do
      case Core.checkProgram (wtCore wt) of
        Right checked
          | any ((==out) . functionReturns . snd) checked ->
             return (Just wt)
        _ ->
          return Nothing

tryGenAttributeFromCore :: Core.Program () Var -> Gen (Maybe WellTypedCluster)
tryGenAttributeFromCore core =
   fromEither <$> tryGenAttributeFromCore' core

tryGenAttributeFromCore' :: Core.Program () Var -> Gen (Either Savage.String WellTypedCluster)
tryGenAttributeFromCore' core
  | sty <- Core.inputType core
  , SumT ErrorT ty <- sty
  = do
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
        let
          coreOutputs =
            fmap (second functionReturns) checked
        _guardCoreOutputs <- traverse (supportedOutputType . snd) coreOutputs

        let
          avalanche =
            testFresh "fromCore" $ A.programFromCore namer core
        flatStmts <- nobodyCares (testFreshT "anf" $ A.flatten () (A.statements avalanche))
        flattened <- nobodyCares (A.checkProgram A.flatFragment (replaceStmts avalanche flatStmts))
        flat <- nobodyCares (testFresh "simp" $ A.simpFlattened dummyAnn A.defaultSimpOpts flattened)
        simplified <- nobodyCares (A.checkProgram A.flatFragment (A.eraseAnnotP flat))

        -- FIXME
        -- Single-kernel cluster only (see comment above).
        let clusterId = Sea.ClusterId 0 -- nobody cares
        cluster <- nobodyCares (Sea.clusterOfPrograms clusterId attribute (NonEmpty.fromList [simplified]))

        let
          wta = WellTypedCluster {
            wtCluster       = cluster
          , wtFactType      = ty
          , wtOutputs       = coreOutputs
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
genWellTypedWithStruct :: Sea.InputAllowDupTime -> Gen (Maybe WellTyped)
genWellTypedWithStruct allowDupTime = do
  st <- arbitrary :: Gen StructType
  tryGenWellTypedWithInput allowDupTime . SumErrorFactT . StructT $ st

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
      -- trace ("*** DISCARD: " <> show t) $
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

  -- Bufs should not occur in the output of either Core or Avalanche.
  BufT{} ->
    False

  -- We don't guarantee that serialisation of empty structs in PSV
  -- match Core output, and we never will because Zebra doesn't.
  -- We don't acceppt empty structs as input and there is no way
  -- to construct empty structs, but randomly generated Core programs
  -- might contain some.
  StructT s
    | StructType m <- s
    , not (Map.null m)
    ->  isSupportedStructOutputType s
    | otherwise
    -> False

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
  maxMapSize <- getPositive <$> arbitrary
  return $ Sea.PsvConstants maxRowCount inputBuf outputBuf factsLimit maxMapSize

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
  in [ LT.fromStrict (getEntity e), LT.fromStrict (renderInputName a), valueText, timeText ]

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
textOfInputTime = LT.fromStrict . renderTime

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

sortFacts :: [WellTypedValue] -> [WellTypedValue]
sortFacts =
  List.sortBy (comparing (eavtEntity &&& eavtInputName &&& (atTime . eavtValue)))
