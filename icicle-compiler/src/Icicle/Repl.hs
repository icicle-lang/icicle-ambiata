{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Repl (
    ReplError (..)
  , module P
  , annotOfError
  , sourceParse
  , sourceDesugar
  , sourceReify
  , sourceCheck
  , sourceConvert
  , checkAvalanche
  , P.coreAvalanche
  , P.flattenAvalanche
  , coreFlatten
  , coreFlatten_
  , P.simpAvalanche
  , P.simpFlattened
  , P.sourceInline
  , P.coreSimp
  , readFacts
  , readIcicleLibrary

  , DictionaryLoadType(..)
  , loadDictionary
  ) where

import qualified Icicle.Avalanche.Simp            as AS
import qualified Icicle.Avalanche.Prim.Flat       as APF

import           Icicle.Data
import qualified Icicle.Dictionary                as D
import           Icicle.Internal.Pretty
import qualified Icicle.Pipeline                  as P
import qualified Icicle.Serial                    as S
import qualified Icicle.Simulator                 as S
import qualified Icicle.Source.Checker            as SC
import qualified Icicle.Source.Type               as ST
import qualified Icicle.Storage.Dictionary.TextV1 as DictionaryText
import qualified Icicle.Storage.Dictionary.Toml   as DictionaryToml

import           P

import           Control.Monad.Trans.Class

import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Data.Traversable                 as TR

import           System.IO

import qualified Text.ParserCombinators.Parsec    as Parsec

import           X.Control.Monad.Trans.Either


type Var = P.SourceVar

data ReplError
 = ReplErrorCompileCore      (P.CompileError     Var)
 | ReplErrorCompileAvalanche (P.CompileError     Var)
 | ReplErrorRuntime          (S.SimulateError () Var)
 | ReplErrorDictionaryLoad   DictionaryToml.DictionaryImportError
 | ReplErrorDecode           S.ParseError
 deriving (Show)

annotOfError :: ReplError -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ReplErrorCompileCore d
     -> P.annotOfError d
    ReplErrorCompileAvalanche _
     -> Nothing
    ReplErrorRuntime _
     -> Nothing
    ReplErrorDictionaryLoad _
     -> Nothing
    ReplErrorDecode  _
     -> Nothing

instance Pretty ReplError where
 pretty e
  = case e of
     ReplErrorCompileCore d
      -> pretty d
     ReplErrorCompileAvalanche d
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

data DictionaryLoadType
 = DictionaryLoadTextV1 FilePath
 | DictionaryLoadToml   FilePath
 deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- * Check and Convert

sourceParse :: Text -> Either ReplError (P.QueryTopPosUntyped Var)
sourceParse = first ReplErrorCompileCore . P.sourceParseQT "repl" (Namespace "namespace-repl")

sourceDesugar :: P.QueryTopPosUntyped Var -> Either ReplError (P.QueryTopPosUntyped Var)
sourceDesugar = first ReplErrorCompileCore . P.sourceDesugarQT

sourceReify :: P.QueryTopPosTyped Var -> P.QueryTopPosTyped Var
sourceReify = P.sourceReifyQT

sourceCheck :: SC.CheckOptions -> D.Dictionary -> P.QueryTopPosUntyped Var -> Either ReplError (P.QueryTopPosTyped Var, ST.Type Var)
sourceCheck opts d
 = first ReplErrorCompileCore . P.sourceCheckQT opts d

sourceConvert :: D.Dictionary -> P.QueryTopPosTyped Var -> Either ReplError (P.CoreProgramUntyped Var)
sourceConvert d
 = first ReplErrorCompileCore . P.sourceConvert d

coreFlatten :: P.CoreProgramUntyped Var -> Either ReplError (P.AvalProgramUntyped Var APF.Prim)
coreFlatten = first ReplErrorCompileAvalanche . P.coreFlatten

coreFlatten_ :: AS.SimpOpts -> P.CoreProgramUntyped Var -> Either ReplError (P.AvalProgramUntyped Var APF.Prim)
coreFlatten_ opts
 = first ReplErrorCompileAvalanche . P.coreFlatten_ opts

checkAvalanche :: P.AvalProgramUntyped Var APF.Prim -> Either ReplError (P.AvalProgramTyped Var APF.Prim)
checkAvalanche
 = first ReplErrorCompileAvalanche . P.checkAvalanche

readFacts :: D.Dictionary -> Text -> Either ReplError [AsAt Fact]
readFacts dict raw
  = first ReplErrorDecode
  $ TR.traverse (S.decodeEavt dict) $ T.lines raw

loadDictionary :: SC.CheckOptions -> DictionaryLoadType -> EitherT ReplError IO D.Dictionary
loadDictionary checkOpts load
 = case load of
    DictionaryLoadTextV1 fp
     -> do  raw <- lift $ T.readFile fp
            ds  <- firstEitherT ReplErrorDecode
                 $ hoistEither
                 $ TR.traverse DictionaryText.parseDictionaryLineV1
                 $ T.lines raw

            return $ D.Dictionary ds []

    DictionaryLoadToml fp
     -> firstEitherT ReplErrorDictionaryLoad $ DictionaryToml.loadDictionary checkOpts DictionaryToml.ImplicitPrelude fp

readIcicleLibrary :: Parsec.SourceName -> Text -> Either ReplError (P.FunEnvT Parsec.SourcePos Var)
readIcicleLibrary n = first ReplErrorCompileCore . P.readIcicleLibrary "repl" n
