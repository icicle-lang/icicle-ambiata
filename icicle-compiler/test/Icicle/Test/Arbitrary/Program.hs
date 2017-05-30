{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Program where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import           Icicle.Data (Entity(..), Attribute(..), AsAt(..))
import           Icicle.BubbleGum (BubbleGumFact(..), Flavour(..))
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

import qualified Prelude as Savage

import Text.Show.Pretty (ppShow)


newtype InputType = InputType {
    unInputType :: ValType
  } deriving (Show)

data WellTyped = WellTyped {
    wtEntities      :: [Entity]
  , wtAttribute     :: Attribute
  , wtFactType      :: ValType
  , wtFacts         :: [AsAt BaseValue]
  , wtTime          :: Time
  , wtMaxMapSize    :: Int
  , wtCore          :: C.Program ()         Var
  , wtAvalanche     :: A.Program ()         Var C.Prim
  , wtAvalancheFlat :: A.Program (Annot ()) Var A.Prim
  }

instance Show WellTyped where
  show wt
    = show
    $ vsep
    [ "well-typed:"
    , "  entities  = " <> pretty (       wtEntities  wt)
    , "  attribute = " <> pretty (       wtAttribute wt)
    , "  fact type = " <> pretty (       wtFactType  wt)
    , "  facts     = " <> text   (show $ wtFacts     wt)
    , "  time      = " <> text   (show $ wtTime      wt)
    , "  core      ="
    , indent 4 $ pretty (wtCore      wt)
    , "  avalanche ="
    , indent 4 $ pretty (wtAvalanche wt)
    , "  flat      ="
    , indent 4 $ pretty (wtAvalancheFlat wt)
    ]

instance Arbitrary InputType where
  arbitrary = validated 100 $ do
    ty <- arbitrary
    if isSupportedInput ty
       then pure . Just . inputTypeOf $ ty
       else pure Nothing

inputTypeOf :: ValType -> InputType
inputTypeOf ty
    = InputType $ SumT ErrorT ty

instance Arbitrary WellTyped where
  arbitrary = validated 10 (tryGenWellTypedWith S.DoNotAllowDupTime =<< arbitrary)

tryGenWellTypedWith :: S.InputAllowDupTime -> InputType -> Gen (Maybe WellTyped)
tryGenWellTypedWith allowDupTime i@(InputType ty) = do
    core           <- programForStreamType ty
    tryGenWellTypedFromCore allowDupTime i core

tryGenWellTypedWithOutput :: S.InputAllowDupTime -> InputType -> ValType -> Gen (Maybe WellTyped)
tryGenWellTypedWithOutput allowDupTime i out = do
 wt <- tryGenWellTypedWith allowDupTime i
 case wt of
  Just wt' -> return $ do
    checked <- fromEither $ C.checkProgram $ wtCore wt'
    case any ((==out) . functionReturns . snd) $ checked of
     True -> return wt'
     False -> Nothing

  _ -> return Nothing

tryGenWellTypedFromCore :: S.InputAllowDupTime -> InputType -> C.Program () Var -> Gen (Maybe WellTyped)
tryGenWellTypedFromCore allowDupTime ty core
 = fromEither <$> tryGenWellTypedFromCoreEither allowDupTime ty core

tryGenWellTypedFromCoreEither :: S.InputAllowDupTime -> InputType -> C.Program () Var -> Gen (Either Savage.String WellTyped)
tryGenWellTypedFromCoreEither allowDupTime (InputType ty) core = do
    entities       <- List.sort . List.nub . getNonEmpty <$> arbitrary
    attribute      <- arbitrary
    (inputs, time) <- case allowDupTime of
                        S.AllowDupTime
                          -> inputsForType ty
                        S.DoNotAllowDupTime
                          -> first (List.nubBy ((==) `on` atTime)) <$> inputsForType ty
    maxMapSize     <- arbitrary
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

      return WellTyped {
          wtEntities      = entities
        , wtAttribute     = attribute
        , wtFactType      = ty
        , wtFacts         = fmap (fmap snd) inputs
        , wtTime          = time
        , wtMaxMapSize    = maxMapSize
        , wtCore          = core
        , wtAvalanche     = avalanche
        , wtAvalancheFlat = simplified
        }
    where
      replaceStmts prog stms = prog { A.statements = stms }

      namer       = A.namerText (flip Var 0)
      dummyAnn    = Annot (FunT [] UnitT) ()

      supportedOutput t | isSupportedOutput t = Right t
      supportedOutput t                       = Left ("Unsupported output: " <> show t)



genWellTypedWithDuplicateTimes :: Gen WellTyped
genWellTypedWithDuplicateTimes
 = validated 10 (tryGenWellTypedWith S.AllowDupTime =<< arbitrary)

-- If the input are structs, we can pretend it's a dense value
-- We can't treat other values as a single-field dense struct because the
-- generated programs do not treat them as such.
genWellTypedWithStruct :: S.InputAllowDupTime -> Gen WellTyped
genWellTypedWithStruct allowDupTime = validated 10 $ do
  st <- arbitrary :: Gen StructType
  tryGenWellTypedWith allowDupTime (inputTypeOf $ StructT st)

evalWellTyped :: WellTyped -> [(OutputName, BaseValue)]
evalWellTyped wt
 | null $ wtFacts wt
 = []
 | otherwise
 = case PV.eval (wellTypedEvalContext wt) inputs (wtCore wt) of
    Left err -> Savage.error ("Evaluating Core: " <> show err)
    Right r  -> PV.value r
 where
  inputs
   = List.zipWith mkInput [0..] (wtFacts wt)
  mkInput ix (AsAt fact time)
   = AsAt (BubbleGumFact (Flavour ix time), fact) time

wellTypedEvalContext :: WellTyped -> EvalContext
wellTypedEvalContext wt = EvalContext (wtTime wt) (wtMaxMapSize wt) 

------------------------------------------------------------------------

validated :: Int -> Gen (Maybe a) -> Gen a
validated n g
  | n <= 0    = discard
  | otherwise = do
      m <- g
      case m of
        Nothing -> validated (n-1) g
        Just x  -> pure x

fromEither :: Either x a -> Maybe a
fromEither (Left _)  = Nothing
fromEither (Right x) = Just x

nobodyCares :: Show a => Either a b -> Either Savage.String b
nobodyCares = first (ppShow)


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
