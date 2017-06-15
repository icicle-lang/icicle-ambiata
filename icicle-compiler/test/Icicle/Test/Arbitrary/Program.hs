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

import           Data.String
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text

import           Icicle.Common.Eval

import           Icicle.Data (Entity(..), Attribute(..), AsAt(..))
import           Icicle.BubbleGum (BubbleGumFact(..), Flavour(..))
import           Icicle.Data.Time (Time, renderTime)

import qualified Icicle.Core.Program.Program as C
import qualified Icicle.Core.Program.Check as C
import qualified Icicle.Core.Exp.Prim      as C
import qualified Icicle.Core.Eval.Program   as PV

import qualified Icicle.Avalanche.Annot as A
import qualified Icicle.Avalanche.Check as A
import qualified Icicle.Avalanche.FromCore as A
import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Avalanche.Simp as A
import qualified Icicle.Avalanche.Statement.Flatten as A

import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Annot

import           Icicle.Internal.Pretty

import qualified Icicle.Sea.FromAvalanche.Analysis as S
import qualified Icicle.Sea.Eval as S

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary.Data
import           Icicle.Test.Arbitrary.Core

import           P

import           Test.QuickCheck

import           Disorder.Corpus

import qualified Prelude as Savage
import           Text.Show.Pretty (ppShow)


newtype InputType = InputType {
    unInputType :: ValType
  } deriving (Show)

data WellTyped = WellTyped {
    wtEvalContext :: EvalContext
  , wtFacts       :: [WellTypedValue]
  , wtAttributes  :: [WellTypedAttribute]
  }

data WellTypedValue = WellTypedValue {
    eavtEntity    :: !Entity
  , eavtAttribute :: !Attribute
  , eavtValue     :: !(AsAt BaseValue)
  }

-- FIXME
-- This currently generates one compute kernel per attribute only.
-- We leave the testing of multiple compute kernels to PsvFission, which does its own thing.
-- But perhaps it's better to generate multiple compute kernels here too.
data WellTypedAttribute = WellTypedAttribute {
    wtInputId       :: InputId
  , wtFactType      :: !ValType
  , wtInputType     :: !InputType -- SumT ErrorT FactType
  , wtCore          :: !(C.Program () Var)
  , wtAvalanche     :: !(A.Program () Var C.Prim)
  , wtAvalancheFlat :: !(A.Program (Annot ()) Var A.Prim)
  }

instance Show WellTyped where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedValue where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Show WellTypedAttribute where
  show x = displayS (renderPretty 0.8 80 (pretty x)) ""

instance Pretty WellTyped where
  pretty wt =
    vsep $
      [ "Well-typed:"
      , indent 2 $ "Attributes ="
      , indent 4 $ vsep (fmap pretty (wtAttributes wt))
      , indent 2 $ "Chord time  = " <> (text . show . renderTime . evalSnapshotTime . wtEvalContext $ wt)
      , indent 2 $ "Max map size = " <> (text . show . evalMaxMapSize . wtEvalContext $ wt)
      , indent 2 $ "EAVTs ="
      , indent 4 $ vsep (fmap pretty (wtFacts wt))
      ]

instance Pretty WellTypedValue where
  pretty eavt =
    encloseSep lbracket rbracket comma $
      [ pretty (eavtEntity eavt)
      , pretty (eavtAttribute eavt)
      , text . show . atFact . eavtValue $ eavt
      , text . show . renderTime . atTime . eavtValue $ eavt
      ]

instance Pretty WellTypedAttribute where
  pretty wt =
    vsep $
      [ "Input ID = " <> pretty (wtInputId wt)
      , "Fact type = " <> pretty (wtFactType  wt)
      --, "Core ="
      --, indent 2 $ pretty (wtCore wt)
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
    validated 10 (tryGenWellTypedWithInput S.DoNotAllowDupTime =<< arbitrary)

instance Arbitrary WellTypedAttribute where
  arbitrary =
    validated 10 (tryGenAttributeWithInput =<< arbitrary)

genMaxMapSize :: Gen Int
genMaxMapSize =
  (getPositive <$> arbitrary) `suchThat` ((>= 128))

tryGenWellTypedWithInput ::
     S.InputAllowDupTime
  -> InputType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInput allowDupTime ty = do
  as <- validatedNonEmpty 10 (tryGenAttributeWithInput ty)
  let
    replaceAttributeName =
      List.zipWith (\i x -> x { wtAttribute = Attribute ("concrete_" <> Text.pack (show i))}) [(0::Int)..]
    as' =
      replaceAttributeName as
  (facts, time) <- genFactsForAttributes allowDupTime as'
  maxMapSize <- genMaxMapSize
  return . Just . WellTyped (EvalContext time maxMapSize) facts $ as'

tryGenWellTypedWithInputAndOutput ::
     S.InputAllowDupTime
  -> InputType
  -> ValType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWithInputAndOutput allowDupTime inputType outputType = do
  as <- validatedNonEmpty 10 (tryGenAttributeWithInputAndOutput inputType outputType)
  let
    replaceAttributeName =
      List.zipWith (\i x -> x { wtAttribute = Attribute ("concrete_" <> Text.pack (show i))}) [(0::Int)..]
    as' =
      replaceAttributeName as
  (facts, time) <- genFactsForAttributes allowDupTime as'
  maxMapSize <- genMaxMapSize
  return . Just . WellTyped (EvalContext time maxMapSize) facts $ as'

tryGenWellTypedFromCore' ::
     S.InputAllowDupTime
  -> InputType
  -> C.Program () Var
  -> Gen (Either Savage.String WellTyped)
tryGenWellTypedFromCore' allowDupTime ty core = do
  attrs <- validatedNonEmpty' 10 (tryGenAttributeFromCore' ty core)
  case attrs of
    Left e ->
      return (Left e)
    Right as -> do
      (facts, time) <- genFactsForAttributes allowDupTime as
      maxMapSize <- genMaxMapSize
      return . Right . WellTyped (EvalContext time maxMapSize) facts $ as

genWellTypedWithDuplicateTimes :: Gen WellTyped
genWellTypedWithDuplicateTimes
 = validated 10 (tryGenWellTypedWithInput S.AllowDupTime =<< arbitrary)

evalWellTyped :: WellTyped -> Map Entity [(OutputName, BaseValue)]
evalWellTyped wt =
  fmap (foldr (\(n,vs) acc -> acc <> List.zip (List.replicate (length vs) n) vs) [] . Map.toList) .
  Map.unionsWith (Map.unionWith (<>)) .
  fmap (evalWellTypedAttribute (wtEvalContext wt) (wtFacts wt)) .
  wtAttributes $
    wt

evalWellTypedAttribute ::
     EvalContext
  -> [WellTypedValue]
  -> WellTypedAttribute
  -> Map Entity (Map OutputName [BaseValue])
evalWellTypedAttribute evalContext wvs (WellTypedAttribute attr _ _ core _ _)
  | null wvs =
      Map.empty
  | otherwise =
      let
        mkInput ix (WellTypedValue entity a (AsAt fact time)) =
          (a, (entity, AsAt (BubbleGumFact (Flavour ix time), fact) time))

        -- use facts for this attribute only
        inputs' =
          Map.fromListWith (<>) .
          fmap (fmap (:[]) . snd) .
          List.filter ((== attr) . fst) .
          List.zipWith mkInput [0..] $
          List.filter ((== attr) . eavtAttribute) $
            wvs

        boom (Left e) =
          Savage.error
           ("Impossible! Failed to evaluate well-typed Core: " <> show e)
        boom (Right xs) =
          xs

      in
        Map.fromList .
        fmap (second (foldr (\(k,v) m -> Map.insertWith (<>) k [v] m) Map.empty . PV.value)) .
        boom .
        forM (Map.toList inputs') $
          \(entity, values) -> (entity,) <$>
            PV.eval evalContext values core


--------------------------------------------------------------------------------

genVTs ::
     S.InputAllowDupTime
  -> InputType
  -> Gen [AsAt BaseValue]
genVTs allowDupTime (unInputType -> ty) = do
  (inputs, _) <-
    case allowDupTime of
      S.AllowDupTime ->
        inputsForType ty
      S.DoNotAllowDupTime ->
        first (List.sortBy (compare `on` atTime) . List.nubBy ((==) `on` atTime)) <$> inputsForType ty
  let
    valueOrTombstone (VLeft (VError _)) =
      VLeft (VError ExceptTombstone)
    valueOrTombstone v =
      v
  return (fmap (fmap (valueOrTombstone . snd)) $ inputs)

genEAVTs ::
     S.InputAllowDupTime
  -> [(Attribute, InputType)]
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
     S.InputAllowDupTime
  -> [WellTypedAttribute]
  -> Gen ([WellTypedValue], Time)
genFactsForAttributes allowDupTime as = do
  facts <- genEAVTs allowDupTime (fmap (\wt -> (wtAttribute wt, wtInputType wt)) as)
  case facts of
    [] ->
      (facts,) <$> arbitrary
    _ ->
      let
        latestTime =
          List.maximum . fmap (atTime . eavtValue) $ facts
      in
        return (facts, latestTime)

--------------------------------------------------------------------------------

inputTypeOf :: ValType -> InputType
inputTypeOf ty
    = InputType $ SumT ErrorT ty

tryGenAttributeWithInput ::
     InputType
  -> Gen (Maybe WellTypedAttribute)
tryGenAttributeWithInput ty = do
  core <- programForStreamType (unInputType ty)
  tryGenAttributeFromCore ty core

tryGenAttributeWithInputAndOutput ::
     InputType
  -> ValType
  -> Gen (Maybe WellTypedAttribute)
tryGenAttributeWithInputAndOutput i out = do
 wt <- tryGenAttributeWithInput i
 case wt of
  Just wt' -> return $ do
    checked <- fromEither $ C.checkProgram $ wtCore wt'
    case any ((==out) . functionReturns . snd) $ checked of
     True -> return wt'
     False -> Nothing
  _ -> return Nothing

tryGenAttributeFromCore ::
     InputType
  -> C.Program () Var
  -> Gen (Maybe WellTypedAttribute)
tryGenAttributeFromCore ty core =
   fromEither <$> tryGenAttributeFromCore' ty core

tryGenAttributeFromCore' ::
     InputType
  -> C.Program () Var
  -> Gen (Either Savage.String WellTypedAttribute)
tryGenAttributeFromCore' sty core
  | SumT ErrorT ty <- unInputType sty = do
      let
        replaceStmts prog stms
          = prog { A.statements = stms }

        namer =
          A.namerText (flip Var 0)

        dummyAnn =
          Annot (FunT [] UnitT) ()

      attribute <- arbitrary
      return $ do
        checked <- nobodyCares (C.checkProgram core)
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
          S.typesOfProgram $
            simplified

        let
          wta = WellTypedAttribute {
            wtInputId       = attribute
          , wtFactType      = ty
          , wtInputType     = sty
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
genWellTypedWithStruct :: S.InputAllowDupTime -> Gen WellTyped
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
  | isSupportedRightOutputType t =
      Right t
  | otherwise =
      Left ("Unsupported output: " <> show t)

isSupportedTopOutputType :: ValType -> Bool
isSupportedTopOutputType = \case
  SumT ErrorT t ->
    isSupportedRightOutputType t
  _ ->
    False

isSupportedRightOutputType :: ValType -> Bool
isSupportedRightOutputType = \case
  OptionT t ->
    isSupportedRightOutputType t
  PairT a b ->
    isSupportedRightOutputType a && isSupportedRightOutputType b
  SumT a b ->
    isSupportedRightOutputType a && isSupportedRightOutputType b
  ArrayT t ->
    isSupportedRightOutputType t
  BufT _ t ->
    isSupportedBufferElemOutputType t
  MapT k v ->
    isSupportedRightOutputType k && isSupportedRightOutputType v
  StructT s ->
    isSupportedStructOutputType s
  t ->
    isSupportedPrimitiveOutputType t

isSupportedStructOutputType :: StructType -> Bool
isSupportedStructOutputType =
  all isSupportedStructFieldOutputType . Map.elems . getStructType

isSupportedStructFieldOutputType :: ValType -> Bool
isSupportedStructFieldOutputType =
  isSupportedRightOutputType

isSupportedPrimitiveOutputType :: ValType -> Bool
isSupportedPrimitiveOutputType = \case
  BoolT ->
    True
  IntT ->
    True
  DoubleT ->
    True
  TimeT ->
    True
  FactIdentifierT ->
    True
  StringT ->
    True
  ErrorT ->
    True
  _ ->
    False

isSupportedBufferElemOutputType :: ValType -> Bool
isSupportedBufferElemOutputType =
  not . containsBuf

containsBuf :: ValType -> Bool
containsBuf = \case
  BufT{} ->
    True
  ArrayT t ->
    containsBuf t
  SumT a b ->
    containsBuf a || containsBuf b
  _ ->
    False
