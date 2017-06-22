{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Dictionary.Data (
    Dictionary(..)
  , DictionaryInput(..)
  , DictionaryOutput(..)
  , DictionaryFunction(..)
  , InputKey(..)
  , AnnotSource
  , mapOfInputs
  , mapOfOutputs
  , unkeyed
  , tombstonesOfDictionary
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

import           Icicle.Source.Query (Function (..), QueryTop (..))
import qualified Icicle.Source.Query                as SQ
import           Icicle.Source.Lexer.Token
import qualified Icicle.Source.Type                 as ST
import           Icicle.Source.ToCore.Context (FeatureVariable (..)) 
import qualified Icicle.Source.ToCore.Context       as STC

import           Icicle.Encoding

import           Icicle.Internal.Pretty

import           Data.Map (Map)
import qualified Data.Map                           as Map
import           Data.Set (Set)
import           Data.String

import           Text.Parsec.Pos ()

import           P


data Dictionary =
  Dictionary {
      dictionaryInputs :: Map InputId DictionaryInput
    , dictionaryOutputs :: Map OutputId DictionaryOutput
    , dictionaryFunctions :: [DictionaryFunction]
    } deriving (Eq, Show)

data DictionaryInput =
  DictionaryInput {
      inputId :: InputId
    , inputEncoding :: Encoding
    , inputTombstones :: Set Text
    , inputKey :: InputKey AnnotSource Variable
    } deriving (Eq, Show)

data DictionaryOutput =
  DictionaryOutput {
      outputId :: OutputId
    , outputQuery :: QueryTop (ST.Annot SourcePos Variable) Variable
    } deriving (Eq, Show)

data DictionaryFunction =
 DictionaryFunction {
     functionName :: Name Variable
   , functionType :: ST.FunctionType Variable
   , functionDefinition :: Function (ST.Annot SourcePos Variable) Variable
   } deriving (Eq, Show)

-- | The query is keyed by this "virtual key". Facts (for one entity) are nubbed by this key.
newtype InputKey a n =
  InputKey {
      unInputKey :: Maybe (SQ.Exp a n)
    } deriving (Eq, Show)

type AnnotSource = ST.Annot SourcePos Variable

unkeyed :: InputKey AnnotSource Variable
unkeyed = InputKey Nothing

tombstonesOfDictionary :: Dictionary -> Map InputId (Set Text)
tombstonesOfDictionary =
  fmap inputTombstones . dictionaryInputs

mapOfInputs :: [DictionaryInput] -> Map InputId DictionaryInput
mapOfInputs =
  Map.fromList . fmap (\x -> (inputId x, x))

mapOfOutputs :: [DictionaryOutput] -> Map OutputId DictionaryOutput
mapOfOutputs =
  Map.fromList . fmap (\x -> (outputId x, x))

--------------------------------------------------------------------------------

parseFact :: Dictionary -> Fact' -> Either DecodeError Fact
parseFact (Dictionary { dictionaryInputs = dict }) fact'
 = do   def <- maybeToRight
                 (DecodeErrorNotInDictionary attr)
                 (P.find (\(DictionaryInput (InputId _ attr') _ _ _) -> (==) attr attr') dict)
        case def of
         DictionaryInput _ enc ts _
          -> factOf <$> parseValue enc ts (factValue' fact')

 where
  attr = factAttribute' fact'

  factOf v
   = Fact
    { factEntity    = factEntity'    fact'
    , factAttribute = factAttribute' fact'
    , factValue     = v
    }

-- | Get all the features and facts from a dictionary.
--
featureMapOfDictionary :: Dictionary -> STC.Features () Variable (InputKey AnnotSource Variable)
featureMapOfDictionary (Dictionary { dictionaryInputs = ds, dictionaryFunctions = functions })
 = STC.Features
     (Map.fromList $ concatMap mkFeatureContext ds)
     (Map.fromList $ fmap (\x -> (functionName x, functionType x)) functions)
     (Just $ var "now")
 where

  mkFeatureContext
   = let context (attr, key, ty, vars)
           = (attr, STC.FeatureConcrete key ty (STC.FeatureContext vars (var "time")))
     in  fmap context . go

  -- If a dictionary entry is a concrete definition, create a feature context with
  -- implicit names such as `now`, `value`, struct field names, etc.
  go :: DictionaryInput -> [( InputId
                            , InputKey AnnotSource Variable
                            , ST.Type Variable
                            , Map (Name Variable) (FeatureVariable () Variable))]
  go (DictionaryInput iid enc _ key)
   | en@(StructT st@(StructType fs)) <- sourceTypeOfEncoding enc
   = [ ( iid
       , key
       , baseType     $  sumT en
       , Map.fromList $  exps "fields" en
                      <> concatMap (go' Nothing st) (Map.toList fs)
       )
     ]

   | otherwise
   = let e' = sourceTypeOfEncoding enc
     in [ ( iid
          , key
          , baseType $ sumT e'
          , Map.fromList $ exps "value" e' ) ]

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

  xapp
   = X.XApp ()

  exps :: Text -> ValType -> [(Name Variable, FeatureVariable () n)]
  exps str e'
   = [ (var str, STC.FeatureVariable (baseType e') (X.XApp () (xfst (sumT e') TimeT)) True)
     , time_as_snd e'
     , true_when_tombstone e' ]

  time_as_snd :: ValType -> (Name Variable, FeatureVariable () n)
  time_as_snd e'
   = ( var "time"
     , STC.FeatureVariable (baseType TimeT) (X.XApp () (xsnd (sumT e') TimeT)) False)

  true_when_tombstone :: ValType -> (Name Variable, FeatureVariable () n)
  true_when_tombstone e'
   = ( var "tombstone"
     , STC.FeatureVariable (baseType BoolT) (X.XApp () (xtomb e') . X.XApp () (xfst (sumT e') TimeT)) False)

  var :: Text -> Name Variable
  var = nameOf . NameBase . Variable

prettyDictionarySummary :: Dictionary -> Doc
prettyDictionarySummary dict
 = "Dictionary" <> line
 <> indent 2
 (  "Functions" <> line
 <> indent 2 (vcat $ (pprInbuilt <$> SQ.listOfBuiltinFuns) <> (pprFun <$> dictionaryFunctions dict))
 <> line
 <> "Inputs" <> line
 <> indent 2 (vcat $ fmap pprInput $ Map.elems $ dictionaryInputs dict)
 <> "Outputs" <> line
 <> indent 2 (vcat $ fmap pprOutput $ Map.elems $ dictionaryOutputs dict))
 where
  pprInput (DictionaryInput attr enc _ key)
   = padDoc 20 (pretty attr) <> " by " <> pretty key <> " : " <> pretty enc

  pprOutput (DictionaryOutput attr q)
   = padDoc 20 (pretty attr) <> " = " <> indent 0 (pretty q)

  pprFun (DictionaryFunction f t _)
   = padDoc 20 (pretty f) <> " : " <> ST.prettyFunWithLetters t

  pprInbuilt f
   = padDoc 20 (annotate AnnVariable $ pretty f) <> " : " <> (prettyInbuiltType f)

  prettyInbuiltType
   = ST.prettyFunWithLetters
   . snd
   . flip Fresh.runFresh freshNamer
   . SQ.primLookup'
   . SQ.Fun
     where
       freshNamer
        = Fresh.counterPrefixNameState (fromString . show) "inbuilt"

instance Pretty (InputKey AnnotSource Variable) where
 pretty (InputKey Nothing)  = ""
 pretty (InputKey (Just x)) = "(" <> pretty x <> ")"
