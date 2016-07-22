{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}

-- * Components of REPL for the Source language
module Icicle.Repl.Source
  ( SourceReplState (..)
  , SourceError (..)
  , defaultSourceReplState
  , posOfError
  , loadDictionary
  , readIcicleLibrary
  , readNormalisedFacts
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

import           Icicle.Data
import           Icicle.Data.Time

import           Icicle.Dictionary

import qualified Icicle.Storage.Dictionary.TextV1 as DictionaryText
import qualified Icicle.Storage.Dictionary.Toml   as DictionaryToml

import           Icicle.Serial                    (SerialError(..))
import qualified Icicle.Serial                    as Serial

import qualified Icicle.Compiler.Source           as Compiler

import qualified Icicle.Repl.Base                 as Repl


data SourceReplState
   = SourceReplState
   { facts              :: [AsAt NormalisedFact]
   , dictionary         :: Dictionary
   , currentTime        :: Time
   , inlineOpt          :: Compiler.InlineOption
   , hasType            :: Bool
   , hasBigData         :: Bool
   , hasAnnotated       :: Bool
   , hasInlined         :: Bool
   , hasDesugar         :: Bool
   , hasReified         :: Bool
   }

defaultSourceReplState :: SourceReplState
defaultSourceReplState
  = (SourceReplState
       []
       demographics
       (unsafeTimeOfYMD 1970 1 1)
       Compiler.InlineUsingLets
       False False False False False False )
    { hasType = True
    , hasBigData = False }

--------------------------------------------------------------------------------

data SourceError
  = SourceErrorCompile    (Compiler.ErrorSource Compiler.Var)
  | SourceErrorSerial      SerialError
  | SourceErrorDictionary  DictionaryToml.DictionaryImportError

posOfError :: SourceError -> Maybe Parsec.SourcePos
posOfError (SourceErrorCompile e) = Compiler.annotOfError e
posOfError _                      = Nothing

instance Pretty SourceError where
 pretty e
  = case e of
     SourceErrorCompile d
      -> pretty d
     SourceErrorDictionary d
      -> "Dictionary load error:" <> Pretty.line
      <> indent 2 (pretty d)
     SourceErrorSerial d
      -> "Decode error:" <> Pretty.line
      <> indent 2 (pretty d)

loadDictionary :: Check.CheckOptions -> Repl.DictionaryLoadType -> EitherT SourceError IO Dictionary
loadDictionary checkOpts load
 = case load of
    Repl.DictionaryLoadTextV1 fp
     -> do  raw <- lift $ T.readFile fp
            ds  <- firstEitherT SourceErrorSerial
                 $ hoistEither
                 $ TR.traverse DictionaryText.parseDictionaryLineV1
                 $ T.lines raw

            return $ Dictionary ds []

    Repl.DictionaryLoadToml fp
     -> firstEitherT SourceErrorDictionary $ DictionaryToml.loadDictionary checkOpts DictionaryToml.ImplicitPrelude fp

readIcicleLibrary :: Parsec.SourceName -> Text -> Either SourceError (Compiler.FunEnvT Parsec.SourcePos Compiler.Var)
readIcicleLibrary n = first SourceErrorCompile . Compiler.readIcicleLibrary "repl" n

readNormalisedFacts :: Dictionary -> Text -> Either SourceError [AsAt NormalisedFact]
readNormalisedFacts dict raw = first SourceErrorSerial $ Serial.readNormalisedFacts dict raw
