{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell#-}
module Icicle.Test.Language where

import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Avalanche.Annot   as A

import           Icicle.Common.Base

import qualified Icicle.Core.Program.Program as C

import qualified Icicle.Source.Query as S
import qualified Icicle.Source.Type as S

import           Icicle.Data.Name
import qualified Icicle.Data as D

import qualified Icicle.Compiler as P
import qualified Icicle.Compiler.Sea as P
import qualified Icicle.Compiler.Source as Source

import qualified Icicle.Source.Parser  as SP

import           Icicle.Test.Arbitrary

import           Icicle.Internal.Pretty

import           Data.Maybe
import           Data.Bitraversable
import qualified Data.Map  as Map
import qualified Data.Text as Text
import           Test.QuickCheck
import           Test.QuickCheck.Property
import qualified Text.Parsec.Pos as Parsec

import           P
import qualified Prelude as Savage

import           System.IO (IO)

import           X.Control.Monad.Trans.Either
import           Disorder.Core.IO


atLeastOneFact :: WellTyped -> Bool
atLeastOneFact =
  not . null . concat . Map.elems . wtInputs

prop_languages_eval :: Property
prop_languages_eval =
  forAll genSumErrorFactType $ \inputType ->
  forAll (genAttributeWithInput inputType `suchThat` atLeastOneFact) $ \wt ->
  forAll (pure $ mkFacts wt) $ \facts ->
  forAll genEvalContext $ \ctx ->
  testIO $ do
  let
    coreRes = P.coreEval ctx facts dummySourceVar
            $ C.renameProgram sourceNameFromTestName
            $ wtCore wt
    flatRes = P.avalancheEval ctx facts dummySourceVar
            $ A.renameProgram sourceNameFromTestName
            $ A.eraseAnnotP
            $ wtAvalancheFlat wt
  seaRes     <- runEitherT
              $ P.seaEval ctx facts dummySourceVar
              $ A.renameProgram sourceNameFromTestName
              $ wtAvalancheFlat wt
  case coreRes of
    Left err
      -> return
       $ counterexample "Core eval failed"
       $ counterexample (show $ pretty err)
       $ counterexample (show $ pretty (wtCore wt))
       $ failed
    Right retCore
     -> case flatRes of
          Left err
            -> return
             $ counterexample "Flat Avalanche eval failed"
             $ counterexample (show $ pretty err)
             $ counterexample (show $ pretty (wtAvalancheFlat wt))
             $ failed
          Right retFlat
            -> case seaRes of
                 Left err
                   -> return
                    $ counterexample "Sea eval failed"
                    $ counterexample (Text.unpack $ P.renderCompilerSeaError err)
                    $ failed
                 Right retSea
                   -> return
                    $ property
                    $ retCore === retFlat .&&. retFlat === retSea

mkFacts :: WellTyped -> [D.AsAt D.Fact]
mkFacts wt =
  concat . catMaybes . fmap mkAsAt . Map.toList $ wtInputs wt
  where
    mkAsAt (ent, as) =
      for as $ \a ->
        D.AsAt
          <$> (D.Fact ent [inputname|input|] <$> factFromTopCoreValue (D.atFact a))
          <*> pure (D.atTime a)

dummySourceVar :: P.QueryTyped Source.Var
dummySourceVar
  = let x = nameOf $ NameBase $ SP.Variable "dummy"
        pos = Parsec.initialPos "dummy"
        input = [inputid|default:input|]
    in  S.QueryTop
          (D.QualifiedInput input)
          (fromMaybe (Savage.error "dummy") . D.parseOutputId . D.renderInputId $ input)
          (S.Query [] $ S.Var (S.Annot pos S.UnitT []) x)

factFromTopCoreValue :: BaseValue -> Maybe D.Value
factFromTopCoreValue = \case
  VLeft (VError _) ->
    Just D.Tombstone
  VRight x ->
    factFromCoreValue x
  _ ->
    Nothing

factFromCoreValue :: BaseValue -> Maybe D.Value
factFromCoreValue bv = case bv of
  VInt x            ->  Just $ D.IntValue x
  VDouble x         ->  Just $ D.DoubleValue x
  VUnit             ->  Just $ D.Tombstone
  VBool x           ->  Just $ D.BooleanValue x
  VTime x           ->  Just $ D.TimeValue x
  VString x         ->  Just $ D.StringValue x
  VArray xs         ->  D.ListValue . D.List
                    <$> sequence (fmap factFromCoreValue xs)
  VPair x y         ->  D.PairValue
                    <$> factFromCoreValue x
                    <*> factFromCoreValue y
  VLeft _           ->  Nothing
  VRight _          ->  Nothing
  VSome x           ->  factFromCoreValue x
  VNone             ->  Just $ D.Tombstone
  VMap x            ->  D.MapValue
                    <$> (sequence
                     $  fmap (bitraverse factFromCoreValue factFromCoreValue )
                     $  Map.toList x)
  VBuf xs           ->  D.ListValue . D.List
                    <$> sequence (fmap factFromCoreValue xs)
  VError _          ->  Just $ D.Tombstone
  VFactIdentifier _ ->  Just $ D.Tombstone
  VStruct x
   |  Map.null x
   -> Just D.Tombstone
   |  otherwise
   ->  D.StructValue . D.Struct
   <$> sequence
    ( fmap (bisequence . bimap (pure . nameOfStructField) factFromCoreValue)
    $ Map.toList x)

sourceNameFromTestName :: Name Var -> Name Source.Var
sourceNameFromTestName
  = nameOf . NameBase . SP.Variable . Text.pack . show . pretty . nameBase


return []
tests :: IO Bool
tests =
  $checkAllWith TestRunNormal (checkArgsSized 10)
