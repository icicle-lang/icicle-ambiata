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
  , sourceInline
  , coreSimp
  , readFacts
  , readIcicleLibrary

  , DictionaryLoadType(..)
  , loadDictionary
  ) where

import qualified Icicle.Avalanche.Statement.Flatten as AS
import qualified Icicle.Common.Base                 as CommonBase
import qualified Icicle.Common.Fresh                as Fresh
import qualified Icicle.Core.Program.Program        as Core
import qualified Icicle.Core.Program.Simp           as Core
import           Icicle.Data
import qualified Icicle.Dictionary                  as D
import qualified Icicle.Storage.Dictionary.TextV1   as DictionaryText
import qualified Icicle.Storage.Dictionary.Toml     as DictionaryToml
import           Icicle.Internal.Pretty
import qualified Icicle.Serial                      as S
import qualified Icicle.Simulator                   as S
import qualified Icicle.Source.Checker              as SC
import qualified Icicle.Source.Parser               as SP
import qualified Icicle.Source.Query                as SQ
import qualified Icicle.Source.ToCore.Base          as STC
import qualified Icicle.Source.ToCore.ToCore        as STC
import qualified Icicle.Source.Type                 as ST
import qualified Icicle.Source.Transform.Inline     as STI

import           P

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import         X.Control.Monad.Trans.Either

import           Data.Either.Combinators
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                         as T
import qualified Data.Traversable                   as TR
import qualified Data.Map                           as M

import           System.IO

import qualified Text.ParserCombinators.Parsec      as Parsec

data ReplError
 = ReplErrorParse   Parsec.ParseError
 | ReplErrorCheck   (SC.CheckError Parsec.SourcePos Var)
 | ReplErrorConvert (STC.ConvertError Parsec.SourcePos Var)
 | ReplErrorDecode  S.ParseError
 | ReplErrorRuntime S.SimulateError
 | ReplErrorFlatten (AS.FlattenError Text)
 | ReplErrorDictionaryLoad DictionaryToml.DictionaryImportError
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
    ReplErrorDictionaryLoad _
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
     ReplErrorDictionaryLoad d
      -> "Dictionary load error:" <> line
      <> indent 2 (pretty d)

type Var        = SP.Variable
type QueryTop'  = SQ.QueryTop Parsec.SourcePos Var
type QueryTop'T = SQ.QueryTop (ST.Annot Parsec.SourcePos Var) Var
type Program'   = Core.Program Var

data DictionaryLoadType
 = DictionaryLoadTextV1 FilePath
 | DictionaryLoadToml   FilePath
 deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- * Check and Convert

sourceParse :: T.Text -> Either ReplError QueryTop'
sourceParse t
 = mapLeft ReplErrorParse
 $ SP.parseQueryTop t


sourceCheck :: D.Dictionary -> QueryTop' -> Either ReplError (QueryTop'T, ST.Type Var)
sourceCheck d q
 = let d' = D.featureMapOfDictionary d
   in  mapLeft ReplErrorCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "t")
     $ runEitherT
     $ SC.checkQT d' q


sourceConvert :: D.Dictionary -> QueryTop'T -> Either ReplError Program'
sourceConvert d q
 = mapRight snd
 $ mapLeft ReplErrorConvert
 $ conv
 where
  d'        = D.featureMapOfDictionary d
  conv      = Fresh.runFreshT
                (STC.convertQueryTop d' q)
                (freshNamer "conv")


sourceInline :: D.Dictionary -> QueryTop'T -> QueryTop'
sourceInline d q
 = SQ.reannotQT ST.annAnnot
 $ inline q
 where
  funs      = M.map snd
            $ D.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (STI.inlineQT funs q')
                (freshNamer "inline")


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

readFacts :: D.Dictionary -> Text -> Either ReplError [AsAt Fact]
readFacts dict raw
  = mapLeft ReplErrorDecode
  $ TR.traverse (S.decodeEavt dict) $ T.lines raw

loadDictionary :: DictionaryLoadType -> EitherT ReplError IO D.Dictionary
loadDictionary load
 = case load of
    DictionaryLoadTextV1 fp
     -> do  raw <- lift $ T.readFile fp
            ds  <- firstEitherT ReplErrorDecode
                 $ hoistEither
                 $ TR.traverse DictionaryText.parseDictionaryLineV1
                 $ T.lines raw

            return $ D.Dictionary ds M.empty

    DictionaryLoadToml fp
     -> do  firstEitherT ReplErrorDictionaryLoad $ DictionaryToml.loadDictionary fp

readIcicleLibrary
    :: Text
    -> Either ReplError
      (M.Map (CommonBase.Name Var)
             ( ST.FunctionType Var
             , SQ.Function (ST.Annot Parsec.SourcePos Var) Var))
readIcicleLibrary input
 = do
  input' <- mapLeft ReplErrorParse $ SP.parseFunctions input
  (tys,bodies)
        <- mapLeft ReplErrorCheck
         $ snd
         $ flip Fresh.runFresh (freshNamer "t")
         $ runEitherT
         $ SC.checkFs M.empty input'
  return (M.intersectionWith (,) tys $ M.fromList bodies)
