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
import           Icicle.Common.Eval
import           Icicle.Common.Type
import           Icicle.Data hiding (StructField)
import           Icicle.Data.Time
import           Icicle.Internal.Pretty
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
import qualified Icicle.Compiler as Compiler

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import           Data.Maybe
import           Data.String
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import           Text.Show.Pretty (ppShow)

import           P

import qualified Prelude as Savage

import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.IO
import           System.IO.Temp (createTempDirectory)

import           Disorder.Corpus

import           Test.QuickCheck (Gen, Arbitrary(..), Positive(..), elements, suchThat)

import           X.Control.Monad.Trans.Either (EitherT, bracketEitherT')


-- | inputType = SumT ErrorT factType
newtype SumErrorFactT = SumErrorFactT {
    factType :: ValType
  } deriving (Show)

data WellTyped =
  WellTyped {
      wtInputType :: !ValType -- clusterInputType == SumT ErrorT wtInputType
    , wtInputs :: !(Map Entity [AsAt BaseValue])
    , wtOutputs :: [(OutputId, ValType)]
    , wtCore :: !(Core.Program () Var)
    , wtAvalanche :: !(A.Program () Var Core.Prim)
    , wtAvalancheFlat :: !(A.Program (Annot ()) Var A.Prim)
    }

instance Show WellTyped where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Pretty WellTyped where
  pretty wt =
    vsep $
      [ "========================================"
      , "== Well-typed"
      , ""
      , "Input Type ="
      , indent 2 $ pretty (wtInputType wt)
      , "Input Data ="
      , indent 2 $ vsep (fmap prettyValues $ Map.toList (wtInputs wt))
      , "Outputs ="
      , indent 2 $ vsep (fmap pretty (wtOutputs wt))
      , "Core ="
      , indent 2 $ pretty (wtCore wt)
      , "Avalanche ="
      , indent 2 $ pretty (wtAvalanche wt)
      , "Flat ="
      , indent 2 $ pretty (wtAvalancheFlat wt)
      ]

prettyValues :: (Entity, [AsAt BaseValue]) -> Doc
prettyValues (e, xts) =
  vsep $ [
      "#" <+> pretty e
    ] <>
    fmap (\(AsAt x t) -> pretty (renderTime t) <> "|" <> pretty x) xts

--------------------------------------------------------------------------------

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
  tryGenWellTypedWithInput =<< (SumErrorFactT <$> arbitrary)

genWellTypedCluster :: Gen (Maybe WellTyped)
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
    List.zipWith fieldName [(0::Int)..] <$>
      listOfN 1 5 genNoListFactType

tryGenWellTypedWithInput ::
     SumErrorFactT
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInput ty =
  tryGenAttributeWithInput ty

tryGenWellTypedWithInputAndOutput ::
     SumErrorFactT
  -> ValType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInputAndOutput inputType outputType = do
  tryGenAttributeWithInputAndOutput inputType outputType

tryGenWellTypedFromCore' ::
     Core.Program () Var
  -> Gen (Either Savage.String WellTyped)
tryGenWellTypedFromCore' core = do
  tryGenAttributeFromCore' core

--------------------------------------------------------------------------------

genEvalContext :: Gen EvalContext
genEvalContext =
  EvalContext
    <$> arbitrary
    <*> (getPositive <$> arbitrary)

--------------------------------------------------------------------------------

-- | This is used to ensure the Core evaluation semantics matches the C code.
--
evalWellTyped ::
     EvalContext
  -> WellTyped
  -> Map Entity [(OutputId, BaseValue)]
evalWellTyped ctx wt =
  let
    evalCore baseValues =
      let
        asStreamValue (AsAt v t) =
          AsAt (BubbleGumFact (Flavour 0 t), v) t
      in
        case Core.eval ctx (fmap asStreamValue baseValues) (wtCore wt) of
          Left e ->
            Savage.error . show . vsep $
              [ "Impossible! Failed to evaluate well-typed Core with this input: "
              , pretty e
              , vsep (fmap (text . show) baseValues)
              ]
          Right xs ->
            xs

    clusterEntities =
      Map.keys $ wtInputs wt

    computeFacts =
      fmap sortByTime $ wtInputs wt

    -- NOTE (invariant)
    -- any entity that was read has an output in psv/zebra
    -- the output handles this logic so we need to emulate that here.

    computeResults =
      fmap (Core.value . evalCore) computeFacts

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
      flip Map.filterWithKey computeResults $ \_k v ->
        not (null (List.filter (hasOutput . snd) v))

    noOutputs =
      flip List.filter clusterEntities $ \e ->
        not (e `elem` Map.keys hasOutputs)

    fillers =
      Map.fromList . fmap (, defaultOuputsForCluster) $ noOutputs

  in
    hasOutputs <> fillers


--------------------------------------------------------------------------------

genVTs :: ValType -> Gen [AsAt BaseValue]
genVTs ft = do
  (inputs, _) <- inputsForType (SumT ErrorT ft)
  let
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
  in List.sort . fmap Entity <$> listOfN 1 10 (elements (southpark <> simpsons <> icicle))

genInputsForType :: ValType -> Gen (Map Entity [AsAt BaseValue])
genInputsForType streamFactType = do
  entities <- genEs
  fmap Map.fromList . forM entities $ \entity ->
    (entity, ) <$> genVTs streamFactType

--------------------------------------------------------------------------------

genAttributeWithInput :: SumErrorFactT -> Gen WellTyped
genAttributeWithInput ty =
  fmap fromJust (tryGenAttributeWithInput ty `suchThat` isJust)

--
-- NOTE
-- May discard because of: output type.
--
tryGenAttributeWithInput :: SumErrorFactT -> Gen (Maybe WellTyped)
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
tryGenAttributeWithInputAndOutput :: SumErrorFactT -> ValType -> Gen (Maybe WellTyped)
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

tryGenAttributeFromCore :: Core.Program () Var -> Gen (Maybe WellTyped)
tryGenAttributeFromCore core =
   fromEither <$> tryGenAttributeFromCore' core

tryGenAttributeFromCore' :: Core.Program () Var -> Gen (Either Savage.String WellTyped)
tryGenAttributeFromCore' core
  | SumT ErrorT ty <- Core.inputType core
  = do
      let
        replaceStmts prog stms
          = prog { A.statements = stms }

        namer =
          A.namerText (flip Var 0)

        dummyAnn =
          Annot (FunT [] UnitT) ()

      inputs <- genInputsForType ty

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

        let
          wta = WellTyped {
            wtInputType =
              ty
          , wtInputs =
              inputs
          , wtOutputs =
              coreOutputs
          , wtCore =
              core
          , wtAvalanche =
              avalanche
          , wtAvalancheFlat =
              simplified
          }

        return wta

  | otherwise =
      Savage.error "Impossible! Generator given an input type that is not Sum Error."

------------------------------------------------------------------------

fromEither :: Either x a -> Maybe a
fromEither (Left _)  = Nothing
fromEither (Right x) = Just x

nobodyCares :: Show a => Either a b -> Either Savage.String b
nobodyCares = first (ppShow)

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

withSystemTempDirectory :: FilePath -> (FilePath -> EitherT e IO a) -> EitherT e IO a
withSystemTempDirectory template action = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release action


genMissingValue :: Gen (Maybe Text)
genMissingValue = elements [Nothing, Just "NA", Just ""]

tombstone :: Text
tombstone = "💀"

sortByTime :: [AsAt BaseValue] -> [AsAt BaseValue]
sortByTime =
  List.sortBy (comparing atTime)
