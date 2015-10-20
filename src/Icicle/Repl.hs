{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Repl (
    ReplError (..)
  , annotOfError
  , sourceParse
  , sourceDesugar
  , sourceReify
  , sourceCheck
  , sourceConvert
  , P.sourceInline
  , P.coreSimp
  , readFacts
  , readIcicleLibrary

  , DictionaryLoadType(..)
  , loadDictionary
  ) where

import qualified Icicle.Common.Base               as CommonBase
import qualified Icicle.Common.Fresh              as Fresh
import           Icicle.Data
import qualified Icicle.Dictionary                as D
import           Icicle.Internal.Pretty
import qualified Icicle.Pipeline                  as P
import qualified Icicle.Serial                    as S
import qualified Icicle.Simulator                 as S
import qualified Icicle.Source.Checker            as SC
import qualified Icicle.Source.Parser             as SP
import qualified Icicle.Source.Query              as SQ
import qualified Icicle.Source.Type               as ST
import qualified Icicle.Storage.Dictionary.TextV1 as DictionaryText
import qualified Icicle.Storage.Dictionary.Toml   as DictionaryToml

import           P

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           X.Control.Monad.Trans.Either

import           Data.Either.Combinators
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Data.Traversable                 as TR

import           System.IO

import qualified Text.ParserCombinators.Parsec    as Parsec

data ReplError
 = ReplErrorCompile        P.CompileError
 | ReplErrorRuntime        (S.SimulateError ())
 | ReplErrorDictionaryLoad DictionaryToml.DictionaryImportError
 | ReplErrorDecode         S.ParseError
 deriving (Show)

annotOfError :: ReplError -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ReplErrorCompile d
     -> P.annotOfError d
    ReplErrorRuntime _
     -> Nothing
    ReplErrorDictionaryLoad _
     -> Nothing
    ReplErrorDecode  _
     -> Nothing

instance Pretty ReplError where
 pretty e
  = case e of
     ReplErrorCompile d
      -> pretty d
     ReplErrorRuntime d
      -> "Runtime error:" <> line
      <> indent 2 (pretty d)
     ReplErrorDictionaryLoad d
      -> "Dictionary load error:" <> line
      <> indent 2 (pretty d)
     ReplErrorDecode d
      -> "Decode error:" <> line
      <> indent 2 (pretty d)

type Var        = SP.Variable

data DictionaryLoadType
 = DictionaryLoadTextV1 FilePath
 | DictionaryLoadToml   FilePath
 deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- * Check and Convert

sourceParse :: Text -> Either ReplError P.QueryTop'
sourceParse = mapLeft ReplErrorCompile . P.sourceParseQT "repl"

sourceDesugar :: P.QueryTop' -> Either ReplError P.QueryTop'
sourceDesugar = mapLeft ReplErrorCompile . P.sourceDesugarQT

sourceReify :: P.QueryTop'T -> Either ReplError P.QueryTop'T
sourceReify = return . P.sourceReifyQT

sourceCheck :: D.Dictionary -> P.QueryTop' -> Either ReplError (P.QueryTop'T, ST.Type SP.Variable)
sourceCheck d
 = mapLeft ReplErrorCompile . P.sourceCheckQT d

sourceConvert :: D.Dictionary -> P.QueryTop'T -> Either ReplError P.Program'
sourceConvert d
 = mapLeft ReplErrorCompile . P.sourceConvert d

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

            return $ D.Dictionary ds []

    DictionaryLoadToml fp
     -> firstEitherT ReplErrorDictionaryLoad $ DictionaryToml.loadDictionary fp

readIcicleLibrary
    :: Parsec.SourceName
    -> Text
    -> Either ReplError
          [ (CommonBase.Name Var
            , ( ST.FunctionType Var
              , SQ.Function (ST.Annot Parsec.SourcePos Var) Var)) ]
readIcicleLibrary source input
 = do input' <- mapLeft (ReplErrorCompile . P.CompileErrorParse) $ SP.parseFunctions source input
      mapLeft (ReplErrorCompile . P.CompileErrorCheck)
             $ snd
             $ flip Fresh.runFresh (P.freshNamer "repl")
             $ runEitherT
             $ SC.checkFs [] input'
