{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Dictionary.Data (
    Dictionary (..)
  , DictionaryEntry (..)
  , DictionaryEntry' (..)
  , Definition (..)
  , Definition' (..)
  , Virtual (..)
  , Virtual' (..)
  , getVirtualFeatures
  , featureMapOfDictionary
  , parseFact
  ) where

import           Icicle.Data

import qualified Icicle.Common.Exp.Prim.Minimal     as X
import qualified Icicle.Common.Exp                  as X
import qualified Icicle.Core                        as X
import           Icicle.Common.Type

import           Icicle.Source.Query
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST

import qualified Icicle.Source.ToCore.Context       as STC

import           Text.Parsec.Pos ()

import           Icicle.Encoding

import qualified Data.Map                           as Map

import           P

data Dictionary =
  Dictionary [DictionaryEntry]
  deriving (Eq, Show)

data DictionaryEntry =
  DictionaryEntry Attribute Definition
  deriving (Eq, Show)

data Definition =
    ConcreteDefinition Encoding
  | VirtualDefinition  Virtual
  deriving (Eq, Show)

-- A parsed and typechecked source program.
newtype Virtual = Virtual {
    unVirtual :: QueryTop (SourcePos, ST.UniverseType Variable) Variable
  } deriving (Eq, Show)

-- Intermediate states so that parsing can be pure.
-- Will need to typecheck once flow through and imports are done.
data DictionaryEntry' =
  DictionaryEntry' Attribute Definition'
  deriving (Eq, Show)

data Definition' =
    ConcreteDefinition' Encoding
  | VirtualDefinition'  Virtual'
  deriving (Eq, Show)

-- A parsed, but still to be typechecked source program.
newtype Virtual' = Virtual' {
    unVirtual' :: QueryTop SourcePos Variable
  } deriving (Eq, Show)

-- | Get all virtual features from dictionary
getVirtualFeatures :: Dictionary -> [(Attribute, Virtual)]
getVirtualFeatures (Dictionary fs)
 = P.concatMap getV fs
 where
  getV (DictionaryEntry a (VirtualDefinition v))
   = [(a,v)]
  getV _
   = []

parseFact :: Dictionary -> Fact' -> Either DecodeError Fact
parseFact (Dictionary dict) fact'
 = do   def <- maybeToRight (DecodeErrorNotInDictionary attr)
                            (P.find (\(DictionaryEntry attr' _) -> (==) attr attr') dict)
        case def of
         DictionaryEntry _ (ConcreteDefinition enc)
          -> factOf <$> parseValue enc (value' fact')
         DictionaryEntry _ (VirtualDefinition _)
          -> Left (DecodeErrorValueForVirtual attr)

 where
  attr = attribute' fact'

  factOf v
   = Fact
    { entity    = entity'    fact'
    , attribute = attribute' fact'
    , value     = v
    }

featureMapOfDictionary :: Dictionary -> STC.Features Variable
featureMapOfDictionary (Dictionary ds)
 = Map.fromList
 $ concatMap go
   ds
 where
  go (DictionaryEntry (Attribute attr) (ConcreteDefinition enc))
   | StructT st@(StructType fs) <- sourceTypeOfEncoding enc
   = let e' = StructT st
     in [ ( Variable attr
        , ( baseType e'
        , Map.fromList
        $ exps "fields" e'
        <> (fmap (\(k,t)
        -> ( Variable $ nameOfStructField k
           , (baseType t, X.XApp (xget k t st) . X.XApp (xfst e' DateTimeT)))
        )
        $ Map.toList fs)))]

   | otherwise
   = let e' = sourceTypeOfEncoding enc
     in [ ( Variable attr
        , ( baseType e'
        , Map.fromList $ exps "value" e'))]
  go _
   = []

  baseType = typeOfValType

  xfst t1 t2
   = X.XPrim (X.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim (X.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)
  xget f t fs
   = X.XPrim (X.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)

  exps str e'
   = [ (Variable str, ( baseType e', X.XApp (xfst e' DateTimeT)))
     , date_as_snd e']
  date_as_snd e'
   = (Variable "date" , ( baseType DateTimeT, X.XApp (xsnd e' DateTimeT)))
