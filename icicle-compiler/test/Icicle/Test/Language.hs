{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Language where

import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Avalanche.Annot   as A

import           Icicle.Common.Base

import qualified Icicle.Core.Program.Program as C

import qualified Icicle.Source.Query as S
import qualified Icicle.Source.Type as S

import qualified Icicle.Data as D

import qualified Icicle.Pipeline as P

import qualified Icicle.Source.Parser  as SP

import           Icicle.Test.Arbitrary

import           Icicle.Internal.Pretty

import           Data.Bitraversable
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as Text
import           Test.QuickCheck
import           Test.QuickCheck.Property
import qualified Text.Parsec.Pos as Parsec

import           P
import           X.Control.Monad.Trans.Either
import           Disorder.Core.IO


prop_languages_eval ewt = testIO $ do
  let wt      = welltyped ewt
      time    = wtTime wt
      facts   = wtEvalFacts ewt
      q       = wtEvalDummyQuery ewt
      coreRes = P.coreEval time facts q
              $ C.renameProgram sourceNameFromTestName
              $ wtCore wt
      flatRes = P.avalancheEval time facts q
              $ A.renameProgram sourceNameFromTestName
              $ A.eraseAnnotP
              $ wtAvalancheFlat wt
  seaRes     <- runEitherT
              $ P.seaEval time facts q
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
                    $ counterexample (show $ pretty err)
                    $ failed
                 Right retSea
                   -> return
                    $ property
                    $ retCore === retFlat .&&. retFlat === retSea

data EvalWellTyped = EvalWellTyped
  { welltyped        :: WellTyped
  , wtEvalFacts      :: [D.AsAt D.Fact]
  , wtEvalDummyQuery :: P.QueryTopPosTyped P.SourceVar
  } deriving (Show)

instance Arbitrary EvalWellTyped where
  arbitrary = do
    wt <- arbitrary
    return $ EvalWellTyped wt (mkFacts wt) (mkDummyQuery wt)

mkFacts :: WellTyped -> [D.AsAt D.Fact]
mkFacts wt
  = catMaybes
  $ List.zipWith (mkAsAt (wtAttribute wt))
                 (wtEntities wt)
                 (wtFacts wt)
  where
    mkAsAt attr ent a
      = D.AsAt <$> (D.Fact ent attr <$> factFromCoreValue (D.atFact a))
               <*> pure (D.atTime a)

mkDummyQuery :: WellTyped -> P.QueryTopPosTyped P.SourceVar
mkDummyQuery wt
  = let x = nameOf $ NameBase $ SP.Variable "dummy"
        pos = Parsec.initialPos "dummy"
    in  S.QueryTop
          (nameOf $ NameBase $ SP.Variable $ D.getAttribute $ wtAttribute wt)
          (OutputName (D.getAttribute $ wtAttribute wt) (D.Namespace "dummy"))
          (S.Query [] $ S.Var (S.Annot pos S.UnitT []) x)

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
    ( fmap (sequence . first (D.Attribute . nameOfStructField) . second factFromCoreValue)
    $ Map.toList x)

sourceNameFromTestName :: Name Var -> Name P.SourceVar
sourceNameFromTestName
  = nameOf . NameBase . SP.Variable . Text.pack . show . pretty . nameBase


return []
tests = $checkAllWith TestRunMore (checkArgsSized 10)
