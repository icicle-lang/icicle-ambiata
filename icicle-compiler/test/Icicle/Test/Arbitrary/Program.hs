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

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text

import           Icicle.BubbleGum (BubbleGumFact(..), Flavour(..))
import           Icicle.Data (Entity(..), AsAt(..))
import           Icicle.Data.Name
import           Icicle.Data.Time (Time)

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
import           Icicle.Common.Eval
import           Icicle.Common.Type
import           Icicle.Common.Annot

import           Icicle.Internal.Pretty

import qualified Icicle.Sea.FromAvalanche.Analysis as S
import qualified Icicle.Sea.Eval as S

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
    wtChordTime  :: !Time
  , wtFacts      :: [WellTypedValue]
  , wtAttributes :: [WellTypedAttribute]
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
      , indent 2 $ "Chord time  = " <> text (show (wtChordTime wt))
      , indent 2 $ "EAVTs ="
      , indent 4 $ vsep (fmap pretty (wtFacts wt))
      ]

instance Pretty WellTypedValue where
  pretty eavt =
    encloseSep lbracket rbracket comma $
      [ pretty (eavtEntity eavt)
      , pretty (eavtAttribute eavt)
      , text $ show (eavtValue eavt) ]

instance Pretty WellTypedAttribute where
  pretty wt =
    vsep $
      [ "Input ID = " <> pretty (wtInputId wt)
      , "Fact type = " <> pretty (wtFactType  wt)
      , "Core ="
      , indent 2 $ pretty (wtCore wt)
      , "Avalanche ="
      , indent 2 $ pretty (wtAvalanche wt)
      , "Flat ="
      , indent 2 $ pretty (wtAvalancheFlat wt)
      ]
>>>>>>> Test: rework WellTyped generator to generate multiple entities and attributes

instance Arbitrary InputType where
  arbitrary = validated 100 $ do
    ty <- arbitrary
    if isSupportedInput ty
       then pure . Just . inputTypeOf $ ty
       else pure Nothing

instance Arbitrary WellTyped where
  arbitrary =
    validated 10 (tryGenWellTypedWith S.DoNotAllowDupTime =<< arbitrary)

instance Arbitrary WellTypedAttribute where
  arbitrary =
    validated 10 (tryGenAttributeWithInput =<< arbitrary)

tryGenWellTypedWith ::
     S.InputAllowDupTime
  -> InputType
  -> Gen (Maybe WellTyped)
tryGenWellTypedWith allowDupTime ty = do
  as <- validatedNonEmpty 10 (tryGenAttributeWithInput ty)
  let
    replaceAttributeName =
      List.zipWith (\i x -> x { wtAttribute = Attribute ("attribute" <> Text.pack (show i))}) [0..]
    as' =
      replaceAttributeName as
  (facts, time) <- genFactsForAttributes allowDupTime as'
  return . Just . WellTyped time facts $ as'

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
      return . Right . WellTyped time facts $ as

genWellTypedWithDuplicateTimes :: Gen WellTyped
genWellTypedWithDuplicateTimes
 = validated 10 (tryGenWellTypedWith S.AllowDupTime =<< arbitrary)

evalWellTypedCore :: WellTyped -> [(OutputName, BaseValue)]
evalWellTypedCore wt
 | null (wtFacts wt) = []
 | otherwise =
     let
       mkInput ix (WellTypedValue _ attr (AsAt fact time)) =
        (attr, AsAt (BubbleGumFact (Flavour ix time), fact) time)
       inputs =
        List.zipWith mkInput [0..] (wtFacts wt)
       eval (WellTypedAttribute attr _ _ core _ _) =
         let
           xs = fmap snd . List.filter ((== attr) . fst) $ inputs
         in PV.eval (wtChordTime wt) xs core
       boom (Left e) =
         Savage.error ("Impossible! Failed to evaluate well-typed Core: " <> show e <> "\n" <> show wt)
       boom (Right a) =
         PV.value a
     in concatMap (boom . eval) . wtAttributes $ wt

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
  return . fmap (fmap (valueOrTombstone . snd)) $ inputs

genEAVTs ::
     S.InputAllowDupTime
  -> [(Attribute, InputType)]
  -> Gen [WellTypedValue]
genEAVTs allowDupTime tys = do
  e <- Entity <$> elements southpark
  -- es <- arbitrary
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
      return (facts, List.maximum . fmap (atTime . eavtValue) $ facts)

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

        supportedOutput t
          | isSupportedOutput t = Right t
        supportedOutput t =
          Left ("Unsupported output: " <> show t)

      attribute <- arbitrary
      return $ do
        checked <- nobodyCares (C.checkProgram core)
        _       <- traverse (supportedOutput . functionReturns . snd) checked
        let avalanche = testFresh "fromCore" $ A.programFromCore namer core
        flatStmts <- nobodyCares (testFreshT "anf" $ A.flatten () (A.statements avalanche))
        flattened <- nobodyCares (A.checkProgram A.flatFragment (replaceStmts avalanche flatStmts))
        unchecked <- nobodyCares (testFresh "simp" $ A.simpFlattened dummyAnn A.defaultSimpOpts flattened)
        simplified <- nobodyCares (A.checkProgram A.flatFragment (A.eraseAnnotP unchecked))

        let types = Set.toList (S.typesOfProgram simplified)
        case filter (not . isSupportedType) types of
          []    -> Right ()
          ts    -> Left ("Unsupported type: " <> show ts)

        return WellTypedAttribute {
            wtInputId       = attribute
          , wtFactType      = ty
          , wtInputType     = sty
          , wtCore          = core
          , wtAvalanche     = avalanche
          , wtAvalancheFlat = simplified
          }
  | otherwise =
      Savage.error "Impossible! Generated an input type that is not Sum Error."

-- If the input are structs, we can pretend it's a dense value
-- We can't treat other values as a single-field dense struct because the
-- generated programs do not treat them as such.
genWellTypedWithStruct :: S.InputAllowDupTime -> Gen WellTyped
genWellTypedWithStruct allowDupTime = validated 10 $ do
  st <- arbitrary :: Gen StructType
  tryGenWellTypedWith allowDupTime (inputTypeOf $ StructT st)

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

isSupportedInput :: ValType -> Bool
isSupportedInput = \case
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  TimeT     -> True
  FactIdentifierT
            -> True
  StringT   -> True

  UnitT     -> False
  ErrorT    -> False
  BufT{}    -> False
  MapT{}    -> False
  PairT{}   -> False
  SumT{}    -> False
  OptionT{} -> False

  ArrayT t
   -> isSupportedInputElem t

  StructT (StructType fs)
   | Map.null fs
   -> False
   | otherwise
   -> all isSupportedInputField (Map.elems fs)

isSupportedInputElem :: ValType -> Bool
isSupportedInputElem = \case
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  TimeT     -> True
  FactIdentifierT
            -> True
  StringT   -> True

  UnitT     -> False
  ErrorT    -> False
  ArrayT{}  -> False
  BufT{}    -> False
  MapT{}    -> False
  PairT{}   -> False
  SumT{}    -> False
  OptionT{} -> False

  StructT (StructType fs)
   -> all isSupportedInputField (Map.elems fs)

isSupportedInputField :: ValType -> Bool
isSupportedInputField = \case
  BoolT           -> True
  IntT            -> True
  DoubleT         -> True
  TimeT           -> True
  FactIdentifierT -> True
  StringT         -> True
  OptionT BoolT   -> True
  OptionT IntT    -> True
  OptionT DoubleT -> True
  OptionT TimeT   -> True
  OptionT StringT -> True

  UnitT     -> False
  ErrorT    -> False
  ArrayT{}  -> False
  BufT{}    -> False
  MapT{}    -> False
  PairT{}   -> False
  SumT{}    -> False
  OptionT{} -> False

  StructT (StructType fs)
   -> all isSupportedInputField (Map.elems fs)

isSupportedOutput :: ValType -> Bool
isSupportedOutput = \case
  OptionT t              -> isSupportedOutputElem t
  PairT a b              -> isSupportedOutputElem a && isSupportedOutputElem b
  SumT ErrorT t          -> isSupportedOutput t

  ArrayT (ArrayT t)      -> isSupportedOutputElem t
  ArrayT t               -> isSupportedOutputElem t
  MapT k v               -> isSupportedOutputElem k && isSupportedOutput v

  t                      -> isSupportedOutputBase t

isSupportedOutputElem :: ValType -> Bool
isSupportedOutputElem = \case
  OptionT t              -> isSupportedOutputElem t
  PairT a b              -> isSupportedOutputElem a && isSupportedOutputElem b
  SumT ErrorT t          -> isSupportedOutputElem t

  ArrayT _               -> False
  MapT   _ _             -> False

  t                      -> isSupportedOutputBase t

isSupportedOutputBase :: ValType -> Bool
isSupportedOutputBase = \case
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  TimeT     -> True
  FactIdentifierT
            -> True
  StringT   -> True

  UnitT     -> False
  ErrorT    -> False
  BufT{}    -> False
  StructT{} -> False

  _         -> False

isSupportedType :: ValType -> Bool
isSupportedType ty = case ty of
  UnitT     -> True
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  TimeT     -> True
  FactIdentifierT
            -> True
  StringT   -> True
  ErrorT    -> True
  SumT{}    -> True

  BufT _ t  -> not (containsBuf t)
  ArrayT t  -> isSupportedType t

  -- should have been melted
  MapT{}    -> Savage.error ("should have been melted: " <> show ty)
  PairT{}   -> Savage.error ("should have been melted: " <> show ty)
  OptionT{} -> Savage.error ("should have been melted: " <> show ty)
  StructT{} -> Savage.error ("should have been melted: " <> show ty)

containsBuf :: ValType -> Bool
containsBuf = \case
  BufT{}    -> True
  ArrayT t' -> containsBuf t'
  SumT a b  -> containsBuf a || containsBuf b
  _         -> False
