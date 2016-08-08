{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}

module Icicle.Dictionary.Data (
    Dictionary (..)
  , DictionaryEntry (..)
  , Definition (..)
  , Virtual (..)
  , FactMode (..)
  , getVirtualFeatures
  , featureMapOfDictionary
  , parseFact
  , prettyDictionarySummary
  ) where

import           Icicle.Data

import qualified Icicle.Common.Exp.Prim.Minimal     as X
import qualified Icicle.Common.Exp                  as X
import qualified Icicle.Common.Fresh                as Fresh
import qualified Icicle.Core                        as X
import           Icicle.Common.Base
import           Icicle.Common.Type (ValType(..), StructType(..))

import           Icicle.Source.Query
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST

import qualified Icicle.Source.ToCore.Context       as STC

import           Text.Parsec.Pos ()

import           Icicle.Encoding

import           Icicle.Internal.Pretty

import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Data.String

import           P


data Dictionary =
  Dictionary
  { dictionaryEntries   :: [DictionaryEntry]
  , dictionaryFunctions :: [ (Name Variable
                           , ( ST.FunctionType Variable
                             , Function (ST.Annot SourcePos Variable) Variable)) ] }
  deriving (Eq, Show)

data DictionaryEntry =
  DictionaryEntry Attribute Definition Namespace
  deriving (Eq, Show)

data Definition =
    ConcreteDefinition Encoding Tombstones FactMode
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
  getV (DictionaryEntry a (VirtualDefinition v) _)
   = [(a,v)]
  getV _
   = []

parseFact :: Dictionary -> Fact Text -> Either DecodeError (Fact Value, FactMode)
parseFact (Dictionary { dictionaryEntries = dict }) fact
 = do   def <- maybeToRight
                 (DecodeErrorNotInDictionary attr)
                 (P.find (\(DictionaryEntry attr' _ _) -> (==) attr attr') dict)
        case def of
         DictionaryEntry _ (ConcreteDefinition enc ts mode) _
          -> (,mode) . factOf <$> parseValue enc ts (factValue fact)
         DictionaryEntry _ (VirtualDefinition _) _
          -> Left (DecodeErrorValueForVirtual attr)

 where
  attr = factAttribute fact

  factOf v
   = Fact
    { factEntity    = factEntity fact
    , factAttribute = attr
    , factValue     = v
    }

featureMapOfDictionary :: Dictionary -> STC.Features () Variable
featureMapOfDictionary (Dictionary { dictionaryEntries = ds, dictionaryFunctions = functions })
 = STC.Features
 (Map.fromList $ concatMap mkFeatureContext ds)
 (Map.fromList $ fmap (\(a,(b,_)) -> (a,b)) functions)
 (Just $ var "now")
 where

  mkFeatureContext d
   = let context (k, t, e, m)
           = (k, STC.FeatureConcrete t e (STC.FeatureContext m (var "time")))
     in  fmap context (go d)

  go (DictionaryEntry (Attribute attr) (ConcreteDefinition enc _ mode) _)
   | en@(StructT st@(StructType fs)) <- sourceTypeOfEncoding enc
   = [ ( var attr
       , baseType     $  sumT en
       , mode
       , Map.fromList $  exps "fields" en
                      <> concatMap (go' Nothing st) (Map.toList fs)
       )
     ]

   | otherwise
   = let e' = sourceTypeOfEncoding enc
     in [ ( var attr
          , baseType $ sumT e'
          , mode
          , Map.fromList $ exps "value" e' ) ]

  go _
   = []

  go' parentGet parent (fn, ft)
   = let getsum b   = xgetsum b fn ft parent
         n          = nameOfStructField fn
         (this, n') = case parentGet of
                        Nothing        -> (getsum True, n)
                        Just (get, pn) -> (getsum False . get, pn <> "." <> n)
         v           = varOfField this n' ft
     in case ft of
          StructT st@(StructType fs)
            -> v : concatMap (go' (Just (this, n')) st) (Map.toList fs)
          _ -> [ v ]

  varOfField get fn ft
   = ( var fn, STC.FeatureVariable (baseType ft) get True)

  sumT ty  = SumT ErrorT ty
  baseType = ST.typeOfValType

  xfst t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim () (X.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)

  xget f t fs
   = X.XPrim () (X.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)
  xgetsum hasTime f t fs x
   = let e'     = StructT fs
         nVal   = var "_val"
         nErr   = var "_err"
         xval   = X.XVar () nVal
         xcase  = X.XPrim () $ X.PrimFold (X.PrimFoldSum ErrorT e') (SumT ErrorT t)
         xleft  = X.XPrim () $ X.PrimMinimal $ X.PrimConst $ X.PrimConstLeft  ErrorT t
         xright = X.XPrim () $ X.PrimMinimal $ X.PrimConst $ X.PrimConstRight ErrorT t
         xfld   = xget f t fs
         xend   = if hasTime
                  then xfst (SumT ErrorT e') TimeT `xapp` x
                  else x
     in xcase
      `xapp` (X.XLam () nErr ErrorT (xleft `xapp` X.XVar () nErr))
      `xapp` (X.XLam () nVal e'     (xright `xapp` (xfld `xapp` xval)))
      `xapp` xend

  xtomb t1
   = X.XApp () (X.XPrim () (X.PrimMinimal $ X.PrimRelation X.PrimRelationEq $ SumT ErrorT t1))
               (X.XValue () (SumT ErrorT t1) (VLeft $ VError ExceptTombstone))

  xapp = X.XApp ()

  exps str e'
   = [ (var str, STC.FeatureVariable (baseType e') (X.XApp () (xfst (sumT e') TimeT)) True)
     , time_as_snd e'
     , true_when_tombstone e' ]
  time_as_snd e'
   = ( var "time"
     , STC.FeatureVariable (baseType TimeT) (X.XApp () (xsnd (sumT e') TimeT)) False)
  true_when_tombstone e'
   = (var "tombstone"
     , STC.FeatureVariable (baseType BoolT) (X.XApp () (xtomb e') . X.XApp () (xfst (sumT e') TimeT)) False)

  var = nameOf . NameBase . Variable

prettyDictionarySummary :: Dictionary -> Doc
prettyDictionarySummary dict
 = "Dictionary" <> line
 <> indent 2
 (  "Functions" <> line
 <> indent 2 (vcat $ (pprInbuilt <$> listOfBuiltinFuns) <> (pprFun <$> dictionaryFunctions dict))
 <> line
 <> "Features" <> line
 <> indent 2 (vcat $ fmap pprEntry $ dictionaryEntries dict))
 where
  pprEntry (DictionaryEntry attr (ConcreteDefinition enc _ ty) _)
   = padDoc 20 (pretty attr) <> " : " <> pretty enc <+> pretty ty
  pprEntry (DictionaryEntry attr (VirtualDefinition virt) _)
   = padDoc 20 (pretty attr) <> " = " <> indent 0 (pretty virt)

  pprFun (f,(t,_))
   = padDoc 20 (pretty f) <> " : " <> ST.prettyFunWithLetters t

  pprInbuilt f
   = padDoc 20 (annotate AnnVariable $ pretty f) <> " : " <> (prettyInbuiltType f)

  prettyInbuiltType
   = ST.prettyFunWithLetters
   . snd
   . flip Fresh.runFresh freshNamer
   . primLookup'
   . Fun
     where
       freshNamer
        = Fresh.counterPrefixNameState (fromString . show) "inbuilt"

instance Pretty Virtual where
 pretty = pretty . unVirtual

