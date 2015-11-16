{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv where

import           Control.Monad.Trans.Either

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Disorder.Core.IO

import           Icicle.Data
import           Icicle.Internal.Pretty

import qualified Icicle.Core.Program.Program as C
import qualified Icicle.Core.Program.Check as C

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
import qualified Icicle.Common.Fresh as Fresh

import qualified Icicle.Sea.Eval as S
import qualified Icicle.Sea.FromAvalanche.Analysis as S

import           Icicle.Test.Arbitrary
import           Icicle.Test.Core.Arbitrary

import qualified Jetski as J

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property


prop_psv wt = testIO $ do
  let seaProgram = Map.singleton (Attribute "eval") (wtAvalanche wt)
  fleet <- runEitherT (S.seaCompile S.Psv seaProgram)
  case fleet of
    Right _
     -> pure (property succeeded)
    Left (S.SeaJetskiError (J.CompilerError _ src err))
     -> pure
      $ counterexample (show $ pretty src)
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed
    Left err
     -> pure
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed

------------------------------------------------------------------------

newtype InputType = InputType {
    unInputType :: ValType
  } deriving (Show)

data WellTyped = WellTyped {
    wtStreamType :: ValType
  , wtFacts      :: [AsAt BaseValue]
  , wtDateTime   :: DateTime
  , wtCore       :: C.Program ()         Var
  , wtAvalanche  :: A.Program (Annot ()) Var A.Prim
  } deriving (Show)

instance Arbitrary InputType where
  arbitrary = validated $ do
    ty <- arbitrary
    if isSupportedInput ty
       then pure . Just . InputType $ SumT ErrorT ty
       else pure Nothing

instance Arbitrary WellTyped where
  arbitrary = validated $ do
    (InputType ty) <- arbitrary
    (inputs, date) <- inputsForType ty
    core           <- programForStreamType ty
    return $ do
      checked <- fromEither (C.checkProgram core)
      _       <- traverse (supportedOutput . functionReturns . snd) checked

      let avalanche = A.programFromCore namer core

      (_, flatStmts) <- fromEither (Fresh.runFreshT (A.flatten () (A.statements avalanche)) anfCounter)

      flattened <- fromEither (A.checkProgram A.flatFragment (replaceStmts avalanche flatStmts))

      let (_, unchecked) = Fresh.runFresh (A.simpFlattened dummyAnn flattened) simpCounter

      simplified <- fromEither (A.checkProgram A.flatFragment (A.eraseAnnotP unchecked))

      let types = Set.toList (S.typesOfProgram simplified)
      case filter (not . isSupportedType) types of
        []    -> Just ()
        (_:_) -> Nothing

      return WellTyped {
          wtStreamType = ty
        , wtFacts      = fmap (fmap snd) inputs
        , wtDateTime   = date
        , wtCore       = core
        , wtAvalanche  = simplified
        }
    where
      replaceStmts prog stms = prog { A.statements = stms }

      namer       = A.namerText (flip Var 0)
      dummyAnn    = Annot (FunT [] UnitT) ()
      anfCounter  = Fresh.counterNameState (Name . Var "anf")  0
      simpCounter = Fresh.counterNameState (Name . Var "simp") 0

      supportedOutput t | isSupportedOutput t = Just t
      supportedOutput _                       = Nothing

------------------------------------------------------------------------

validated :: Gen (Maybe a) -> Gen a
validated g = do
  m <- g
  case m of
    Nothing -> validated g
    Just x  -> pure x

fromEither :: Either x a -> Maybe a
fromEither (Left _)  = Nothing
fromEither (Right x) = Just x

isSupportedInput :: ValType -> Bool
isSupportedInput = \case
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  DateTimeT -> True
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
  BoolT             -> True
  IntT              -> True
  DoubleT           -> True
  DateTimeT         -> True
  StringT           -> True
  OptionT BoolT     -> True
  OptionT IntT      -> True
  OptionT DoubleT   -> True
  OptionT DateTimeT -> True
  OptionT StringT   -> True

  UnitT     -> False
  ErrorT    -> False
  ArrayT{}  -> False
  BufT{}    -> False
  MapT{}    -> False
  PairT{}   -> False
  SumT{}    -> False
  OptionT{} -> False
  StructT{} -> False

isSupportedOutput :: ValType -> Bool
isSupportedOutput = \case
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  DateTimeT -> True
  StringT   -> True

  UnitT     -> False
  ErrorT    -> False
  ArrayT{}  -> False
  BufT{}    -> False
  MapT{}    -> False
  PairT{}   -> False
  OptionT{} -> False
  StructT{} -> False

  SumT te tx
   | ErrorT <- te
   -> isSupportedOutput tx

   | otherwise
   -> False

isSupportedType :: ValType -> Bool
isSupportedType = \case
  UnitT     -> True
  BoolT     -> True
  IntT      -> True
  DoubleT   -> True
  DateTimeT -> True
  StringT   -> True
  ErrorT    -> True
  MapT{}    -> True
  PairT{}   -> True
  OptionT{} -> True
  StructT{} -> True
  SumT{}    -> True

  BufT _ t          -> not (isBufOrArray t)
  ArrayT (ArrayT t) -> not (isBufOrArray t)
  ArrayT (BufT _ t) -> not (isBufOrArray t)
  ArrayT t          -> not (isBufOrArray t)

isBufOrArray :: ValType -> Bool
isBufOrArray = \case
  BufT{}    -> True
  ArrayT{}  -> True
  _         -> False

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 50, maxDiscardRatio = 10000})
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
