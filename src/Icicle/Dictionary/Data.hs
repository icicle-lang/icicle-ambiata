{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Dictionary.Data (
    Dictionary (..)
  , DictionaryEntry (..)
  , Definition (..)
  , Virtual (..)
  , getVirtualFeatures
  , featureMapOfDictionary
  , parseFact
  , prettyDictionarySummary
  ) where

import           Icicle.Data

import qualified Icicle.Common.Exp.Prim.Minimal     as X
import qualified Icicle.Common.Exp                  as X
import qualified Icicle.Core                        as X
import           Icicle.Common.Base
import           Icicle.Common.Type

import           Icicle.Source.Query
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST

import qualified Icicle.Source.ToCore.Context       as STC

import           Text.Parsec.Pos ()

import           Icicle.Encoding

import           Icicle.Internal.Pretty

import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Data.Text (Text)

import           P

data Dictionary =
  Dictionary
  { dictionaryEntries   :: [DictionaryEntry]
  , dictionaryFunctions :: Map.Map
                            (Name Variable)
                            ( ST.FunctionType Variable
                            , Function (ST.Annot SourcePos Variable) Variable)}
  deriving (Eq, Show)

data DictionaryEntry =
  DictionaryEntry Attribute Definition
  deriving (Eq, Show)

data Definition =
    ConcreteDefinition Encoding Tombstones
  | VirtualDefinition  Virtual
  deriving (Eq, Show)

type Tombstones = Set.Set Text

-- A parsed and typechecked source program.
newtype Virtual = Virtual {
    unVirtual :: QueryTop (ST.Annot SourcePos Variable) Variable
  } deriving (Eq, Show)

-- | Get all virtual features from dictionary
getVirtualFeatures :: Dictionary -> [(Attribute, Virtual)]
getVirtualFeatures (Dictionary { dictionaryEntries = fs })
 = P.concatMap getV fs
 where
  getV (DictionaryEntry a (VirtualDefinition v))
   = [(a,v)]
  getV _
   = []

parseFact :: Dictionary -> Fact' -> Either DecodeError Fact
parseFact (Dictionary { dictionaryEntries = dict }) fact'
 = do   def <- maybeToRight (DecodeErrorNotInDictionary attr)
                            (P.find (\(DictionaryEntry attr' _) -> (==) attr attr') dict)
        case def of
         DictionaryEntry _ (ConcreteDefinition enc ts)
          -> factOf <$> parseValue enc ts (value' fact')
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

featureMapOfDictionary :: Dictionary -> STC.Features () Variable
featureMapOfDictionary (Dictionary { dictionaryEntries = ds, dictionaryFunctions = functions })
 = STC.Features
 (Map.fromList $ concatMap go ds)
 (Map.map fst functions)
 (Just $ var "now")
 where
  go (DictionaryEntry (Attribute attr) (ConcreteDefinition enc _))
   | StructT st@(StructType fs) <- sourceTypeOfEncoding enc
   = let e' = StructT st
     in [ ( var attr
        , ( baseType e'
        , Map.fromList
        $ exps "fields" e'
        <> (fmap (\(k,t)
        -> ( var $ nameOfStructField k
           , (baseType t, X.XApp () (xget k t st) . X.XApp () (xfst e' DateTimeT)))
        )
        $ Map.toList fs)))]

   | otherwise
   = let e' = sourceTypeOfEncoding enc
     in [ ( var attr
        , ( baseType e'
        , Map.fromList $ exps "value" e'))]
  go _
   = []

  baseType = ST.typeOfValType

  xfst t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)
  xget f t fs
   = X.XPrim () (X.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)
  xtomb t1
   = X.XApp () (X.XPrim () (X.PrimMinimal $ X.PrimRelation X.PrimRelationEq t1))
               (X.XValue () t1 (VError ExceptTombstone))

  exps str e'
   = [ (var str, ( baseType e', X.XApp () (xfst e' DateTimeT)))
     , date_as_snd e'
     , true_when_tombstone e' ]
  date_as_snd e'
   = (var "date" , ( baseType DateTimeT, X.XApp () (xsnd e' DateTimeT)))
  true_when_tombstone e'
   = (var "tombstone" , ( baseType BoolT, X.XApp () (xtomb e') . X.XApp () (xfst e' DateTimeT)))

  var = Name . Variable



prettyDictionarySummary :: Dictionary -> Doc
prettyDictionarySummary dict
 = "Dictionary" <> line
 <> indent 2
 (  "Functions" <> line
 <> indent 2 (vcat $ fmap pprFun $ Map.toList $ dictionaryFunctions dict)
 <> line
 <> "Features" <> line
 <> indent 2 (vcat $ fmap pprEntry $ dictionaryEntries dict))
 where
  pprEntry (DictionaryEntry attr (ConcreteDefinition enc _))
   = padDoc 20 (pretty attr) <> " : " <> pretty enc
  pprEntry (DictionaryEntry attr (VirtualDefinition virt))
   = padDoc 20 (pretty attr) <> " = " <> indent 0 (pretty virt)

  pprFun (f,(t,_))
   = padDoc 20 (pretty f) <> " : " <> ST.prettyFunWithLetters t

instance Pretty Virtual where
 pretty = pretty . unVirtual

