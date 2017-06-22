{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}

-- * Components of REPL for the Source language
module Icicle.Repl.Source
  ( SourceReplState (..)
  , ErrorSource (..)
  , defaultSourceReplState
  , posOfError
  , readFacts
  , loadDictionary
  , readIcicleLibrary
  , stateEvalContext
  ) where

import           Control.Monad.Trans.Class

import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Data.Traversable                 as TR

import           System.IO

import qualified Text.ParserCombinators.Parsec    as Parsec

import           P

import           X.Control.Monad.Trans.Either

import           Icicle.Internal.Pretty           (Pretty, pretty, indent)
import qualified Icicle.Internal.Pretty           as Pretty

import qualified Icicle.Source.Checker            as Check

import           Icicle.Common.Eval

import           Icicle.Data
import           Icicle.Data.Time

import           Icicle.Dictionary

import qualified Icicle.Storage.Dictionary.TextV1 as DictionaryText
import qualified Icicle.Storage.Dictionary.Toml   as DictionaryToml

import qualified Icicle.Serial                    as Serial

import qualified Icicle.Compiler.Source           as Compiler

import qualified Icicle.Repl.Base                 as Repl


data SourceReplState
   = SourceReplState
   { facts              :: [AsAt Fact]
   , dictionary         :: Dictionary
   , currentTime        :: Time
   , maxMapSize         :: Int
   , inlineOpt          :: Compiler.InlineOption
   , hasType            :: Bool
   , hasTypeCheckLog    :: Bool
   , hasBigData         :: Bool
   , hasAnnotated       :: Bool
   , hasInlined         :: Bool
   , hasDesugar         :: Bool
   , hasReified         :: Bool
   }

defaultSourceReplState :: SourceReplState
defaultSourceReplState
  = SourceReplState
       []
       demographics
       (unsafeTimeOfYMD 1970 1 1)
       (1024*1024)
       Compiler.InlineUsingSubst
       False False False False False False False

stateEvalContext :: SourceReplState -> EvalContext
stateEvalContext s = EvalContext (currentTime s) (maxMapSize s)

--------------------------------------------------------------------------------

data ErrorSource
  = ErrorCompile    (Compiler.ErrorSource Compiler.Var)
  | ErrorDecode      Serial.ParseError
  | ErrorDictionary  DictionaryToml.DictionaryImportError

posOfError :: ErrorSource -> Maybe Parsec.SourcePos
posOfError (ErrorCompile e) = Compiler.annotOfError e
posOfError _                = Nothing

instance Pretty ErrorSource where
 pretty e
  = case e of
     ErrorCompile d
      -> pretty d
     ErrorDictionary d
      -> "Dictionary load error:" <> Pretty.line
      <> indent 2 (pretty d)
     ErrorDecode d
      -> "Decode error:" <> Pretty.line
      <> indent 2 (pretty d)


readFacts :: Dictionary -> Text -> Either ErrorSource [AsAt Fact]
readFacts dict raw
  = first ErrorDecode
  $ TR.traverse (Serial.decodeEavt dict) $ T.lines raw

loadDictionary :: Check.CheckOptions -> Repl.DictionaryLoadType -> EitherT ErrorSource IO Dictionary
loadDictionary checkOpts load
 = case load of
    Repl.DictionaryLoadTextV1 fp
     -> do  raw <- lift $ T.readFile fp
            ds  <- firstEitherT ErrorDecode
                 $ hoistEither
                 $ TR.traverse DictionaryText.parseDictionaryLineV1
                 $ T.lines raw

            return $ Dictionary (mapOfInputs ds) (mapOfOutputs []) []

    Repl.DictionaryLoadToml fp
     -> firstEitherT ErrorDictionary $ DictionaryToml.loadDictionary checkOpts DictionaryToml.ImplicitPrelude fp

readIcicleLibrary :: Parsec.SourceName -> Text -> Either ErrorSource (Compiler.FunEnvT Parsec.SourcePos Compiler.Var)
readIcicleLibrary n = first ErrorCompile . Compiler.readIcicleLibrary "repl" n

