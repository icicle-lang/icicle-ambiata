{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Repl (
    ReplError (..)
  , annotOfError
  , sourceParse
  , sourceCheck
  , sourceConvert
  , sourceParseConvert
  , coreSimp
  , readFacts
  , readDictionary
  ) where

import qualified Icicle.Avalanche.Statement.Flatten as AS
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Type
import qualified Icicle.Common.Exp.Prim.Minimal     as X
import qualified Icicle.Common.Exp                  as X
import qualified Icicle.Core                        as X
import qualified Icicle.Core.Program.Program        as Core
import qualified Icicle.Core.Program.Simp           as Core
import           Icicle.Data
import qualified Icicle.Dictionary                  as D
import qualified Icicle.Dictionary.Parse            as DP
import qualified Icicle.Encoding                    as E
import           Icicle.Internal.Pretty
import qualified Icicle.Serial                      as S
import qualified Icicle.Simulator                   as S
import qualified Icicle.Source.Checker.Checker      as SC
import qualified Icicle.Source.Checker.Error        as SC
import qualified Icicle.Source.Parser               as SP
import qualified Icicle.Source.Query                as SQ
import qualified Icicle.Source.ToCore.Base          as STC
import qualified Icicle.Source.ToCore.Context       as STC
import qualified Icicle.Source.ToCore.ToCore        as STC
import qualified Icicle.Source.Type                 as ST

import           P

import           Data.Either.Combinators
import qualified Data.Map                           as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Traversable                   as TR

import qualified Text.ParserCombinators.Parsec      as Parsec

data ReplError
 = ReplErrorParse   Parsec.ParseError
 | ReplErrorCheck   (SC.CheckError Parsec.SourcePos Var)
 | ReplErrorConvert (STC.ConvertError Parsec.SourcePos Var)
 | ReplErrorDecode  S.ParseError
 | ReplErrorRuntime S.SimulateError
 | ReplErrorFlatten (AS.FlattenError Text)
 deriving (Show)

annotOfError :: ReplError -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ReplErrorParse sp
     -> Just
      $ Parsec.errorPos sp
    ReplErrorCheck       e'
     -> SC.annotOfError  e'
    ReplErrorConvert     e'
     -> STC.annotOfError e'
    ReplErrorDecode  _
     -> Nothing
    ReplErrorRuntime _
     -> Nothing
    ReplErrorFlatten _
     -> Nothing

instance Pretty ReplError where
 pretty e
  = case e of
     ReplErrorParse p
      -> "Parse error:" <> line
      <> indent 2 (text $ show p)
     ReplErrorCheck ce
      -> "Check error:" <> line
      <> indent 2 (pretty ce)
     ReplErrorConvert ce
      -> "Convert error:" <> line
      <> indent 2 (pretty ce)
     ReplErrorDecode d
      -> "Decode error:" <> line
      <> indent 2 (pretty d)
     ReplErrorRuntime d
      -> "Runtime error:" <> line
      <> indent 2 (pretty d)
     ReplErrorFlatten d
      -> "Flatten error:" <> line
      <> indent 2 (text $ show d)

type Var        = SP.Variable
type QueryTop'  = SQ.QueryTop Parsec.SourcePos Var
type QueryTop'T = SQ.QueryTop (Parsec.SourcePos, ST.UniverseType) Var
type Program'   = Core.Program Var

--------------------------------------------------------------------------------

-- * Check and Convert

sourceParse :: T.Text -> Either ReplError QueryTop'
sourceParse t
 = mapLeft ReplErrorParse
 $ SP.parseQueryTop t


sourceCheck :: D.Dictionary -> QueryTop' -> Either ReplError (QueryTop'T, ST.UniverseType)
sourceCheck d q
 = let d' = featureMapOfDictionary d
   in  mapLeft ReplErrorCheck
     $ SC.checkQT d' q


sourceConvert :: D.Dictionary -> QueryTop'T -> Either ReplError Program'
sourceConvert d q
 = mapRight snd
 $ mapLeft ReplErrorConvert
 $ Fresh.runFreshT (STC.convertQueryTop d' q) (freshNamer "conv")
 where
  d' = featureMapOfDictionary d


sourceParseConvert :: T.Text -> Either ReplError Program'
sourceParseConvert t
 = do   q <- sourceParse t
        (q',_) <- sourceCheck D.demographics q
        sourceConvert D.demographics q'



coreSimp :: Program' -> Program'
coreSimp p
 = snd
 $ Fresh.runFresh (Core.simpProgram p) (freshNamer "simp")


freshNamer :: Text -> Fresh.NameState SP.Variable
freshNamer prefix = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) (SP.Variable prefix)


featureMapOfDictionary :: D.Dictionary -> STC.Features Var
featureMapOfDictionary (D.Dictionary ds)
 = Map.fromList
 $ concatMap go
   ds
 where
  go (D.DictionaryEntry (Attribute attr) (D.ConcreteDefinition enc))
   | StructT st@(StructType fs) <- E.sourceTypeOfEncoding enc
   = let e' = StructT st
     in [ ( SP.Variable attr
        , ( e'
        , Map.fromList
        $ exps "fields" e'
        <> (fmap (\(k,t)
        -> ( SP.Variable $ nameOfStructField k
           , (t, X.XApp (xget k t st) . X.XApp (xfst e' DateTimeT)))
        )
        $ Map.toList fs)))]

   | otherwise
   = let e' = E.sourceTypeOfEncoding enc
     in [ ( SP.Variable attr
        , ( e'
        , Map.fromList $ exps "value" e'))]
  go _
   = []

  xfst t1 t2
   = X.XPrim (X.PrimMinimal $ X.PrimPair $ X.PrimPairFst t1 t2)
  xsnd t1 t2
   = X.XPrim (X.PrimMinimal $ X.PrimPair $ X.PrimPairSnd t1 t2)
  xget f t fs
   = X.XPrim (X.PrimMinimal $ X.PrimStruct $ X.PrimStructGet f t fs)

  exps str e'
   = [ (SP.Variable str, ( e', X.XApp (xfst e' DateTimeT)))
     , date_as_snd e']
  date_as_snd e'
   = (SP.Variable "date" , ( DateTimeT, X.XApp (xsnd e' DateTimeT)))

readFacts :: D.Dictionary -> Text -> Either ReplError [AsAt Fact]
readFacts dict raw
  = mapLeft ReplErrorDecode
  $ TR.traverse (S.decodeEavt dict) $ T.lines raw

readDictionary :: Text -> Either ReplError D.Dictionary
readDictionary raw
  = fmap D.Dictionary $ mapLeft ReplErrorDecode
  $ TR.traverse DP.parseDictionaryLineV1 $ T.lines raw
