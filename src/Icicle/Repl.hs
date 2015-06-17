{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl (
    ReplError (..)
  , sourceParse
  , sourceCheck
  , sourceConvert
  , sourceParseConvert
  , readFacts
  ) where

import           Icicle.BubbleGum
import           Icicle.Common.Base
import qualified Icicle.Common.Fresh           as Fresh
import           Icicle.Common.Type
import qualified Icicle.Core.Program.Program   as Core
import           Icicle.Data
import qualified Icicle.Dictionary             as D
import           Icicle.Internal.Pretty
import qualified Icicle.Serial                 as S
import qualified Icicle.Source.Checker.Checker as SC
import qualified Icicle.Source.Checker.Error   as SC
import qualified Icicle.Source.Parser          as SP
import qualified Icicle.Source.Query           as SQ
import qualified Icicle.Source.ToCore.Base     as STC
import qualified Icicle.Source.ToCore.ToCore   as STC
import qualified Icicle.Source.Type            as ST
import qualified Icicle.Simulator as S
import qualified Icicle.Core.Eval.Program as EP

import           P

import           Data.Either.Combinators
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Traversable              as TR


data ReplError
 = ReplErrorParse   SP.ParseError
 | ReplErrorCheck   (SC.CheckError SP.SourcePos Var)
 | ReplErrorConvert (STC.ConvertError SP.SourcePos Var)
 | ReplErrorDecode  S.ParseError
 | ReplErrorRuntime S.SimulateError
 deriving (Show)

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
      <> indent 2 (text $ show d)
     ReplErrorRuntime d
      -> "Runtime error:" <> line
      <> indent 2 (text $ show d)


type Var        = SP.Variable
type QueryTop'  = SQ.QueryTop SP.SourcePos Var
type QueryTop'T = SQ.QueryTop (SP.SourcePos, ST.UniverseType) Var
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


sourceConvert :: QueryTop'T -> Either ReplError Program'
sourceConvert q
 = mapRight snd
 $ mapLeft ReplErrorConvert
 $ Fresh.runFreshT (STC.convertQueryTop q) namer
 where
  mkName i = Name $ SP.Variable ("v" <> T.pack (show i))
  namer = Fresh.counterNameState mkName 0


sourceParseConvert :: T.Text -> Either ReplError Program'
sourceParseConvert t
 = do   q <- sourceParse t
        (q',_) <- sourceCheck D.demographics q
        sourceConvert q'


featureMapOfDictionary :: D.Dictionary -> Map.Map Var (Map.Map Var ST.BaseType)
featureMapOfDictionary (D.Dictionary ds)
 = Map.fromList
 $ concatMap go
   ds
 where
  go (Attribute attr, D.ConcreteDefinition _enc)
   -- TODO: convert Encoding to feature map
   = [(SP.Variable attr, Map.singleton (SP.Variable "value") IntT)]
  go _
   = []

readFacts :: D.Dictionary -> Text -> Either ReplError [AsAt Fact]
readFacts dict raw
  = mapLeft ReplErrorDecode
  $ TR.traverse (S.decodeEavt dict) $ T.lines raw
