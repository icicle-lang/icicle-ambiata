{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , loadDictionary
  ) where

import           Icicle.Common.Base
import           Icicle.Dictionary.Data
import           Icicle.Internal.Pretty                        hiding ((</>))
import qualified Icicle.Pipeline                               as P
import qualified Icicle.Source.Parser                          as SP
import qualified Icicle.Source.Query                           as SQ
import qualified Icicle.Source.Type                            as ST
import           Icicle.Storage.Dictionary.Toml.Toml
import           Icicle.Storage.Dictionary.Toml.TomlDictionary

import qualified Control.Arrow                                 as A
import qualified Control.Exception                             as E
import           Control.Monad.Trans.Either

import           System.FilePath
import           System.IO

import           Data.Either.Combinators
import qualified Data.Map                                      as M
import qualified Data.Set                                      as S
import qualified Data.Text                                     as T
import qualified Data.Text.IO                                  as T

import qualified Text.Parsec                                   as Parsec

import           P



data DictionaryImportError
  = DictionaryErrorIO         E.SomeException
  | DictionaryErrorParsecTOML Parsec.ParseError
  | DictionaryErrorParsecFunc P.CompileError
  | DictionaryErrorParse      [DictionaryValidationError]
  | DictionaryErrorCheck      P.CompileError
  deriving (Show)

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary dictionary
 = loadDictionary' M.empty mempty [] dictionary

loadDictionary'
  :: M.Map (Name SP.Variable) (ST.FunctionType SP.Variable, SQ.Function (ST.Annot Parsec.SourcePos SP.Variable) SP.Variable)
  -> DictionaryConfig
  -> [DictionaryEntry]
  -> FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary' parentFuncs parentConf parentConcrete fp
 = do
  inputText
    <- EitherT
     $ (A.left DictionaryErrorIO)
     <$> (E.try (readFile fp))

  rawToml
    <- hoistEither
     $ (A.left DictionaryErrorParsecTOML)
     $ Parsec.parse tomlDoc fp inputText

  (conf, definitions')
    <- hoistEither
     $ (A.left DictionaryErrorParse)
     $ toEither
     $ tomlDict parentConf rawToml

  parsedImports     <- parseImports conf rp
  importedFunctions <- loadImports parentFuncs parsedImports

  let concreteDefinitions = foldr remakeConcrete [] definitions'
  let virtualDefinitions' = foldr remakeVirtuals [] definitions'

  let d' = Dictionary (concreteDefinitions <> parentConcrete) importedFunctions

  virtualDefinitions <- checkDefs d' virtualDefinitions'

  loadedChapters
    <- (\fp' ->
         loadDictionary' importedFunctions conf concreteDefinitions (rp </> (T.unpack fp'))
       ) `traverse` (chapter conf)

  let functions = M.unions $ importedFunctions : (dictionaryFunctions <$> loadedChapters)
  let totaldefinitions = concreteDefinitions <> virtualDefinitions <> (join $ dictionaryEntries <$> loadedChapters)

  pure $ Dictionary totaldefinitions functions

    where
      rp = (takeDirectory fp)

      remakeConcrete (DictionaryEntry' a (ConcreteDefinition' _ e t)) cds = (DictionaryEntry a (ConcreteDefinition e $ S.fromList $ toList t)) : cds
      remakeConcrete _ cds = cds

      remakeVirtuals (DictionaryEntry' a (VirtualDefinition' (Virtual' v))) vds = (a, v) : vds
      remakeVirtuals _ vds = vds

parseImports conf rp
 = go `traverse` imports conf
 where
  go fp
   = do let fp'' = T.unpack fp
        importsText
          <- EitherT
           $ A.left DictionaryErrorIO
          <$> E.try (T.readFile (rp </> fp''))
        hoistEither
           $ A.left DictionaryErrorParsecFunc
           $ P.sourceParseF fp'' importsText

loadImports parentFuncs parsedImports
 = hoistEither . mapLeft DictionaryErrorCheck
 $ foldlM go parentFuncs parsedImports
 where
  go env f
   = P.sourceDesugarF f >>= P.sourceCheckF env

checkDefs d defs
 = go `traverse` defs
 where
  go (a, q)
   = do  (checked, _)  <- check' d q
         let inlined    = P.sourceInline d checked
         (checked', _) <- check' d inlined
         pure $ DictionaryEntry a (VirtualDefinition (Virtual checked'))
  check' d'
   = hoistEither . mapLeft DictionaryErrorCheck . P.sourceCheckQT d'


instance Pretty DictionaryImportError where
  pretty (DictionaryErrorIO e)
   = "IO Exception:" <+> (text . show) e
  pretty (DictionaryErrorParsecTOML e)
   = "TOML parse error:" <+> (text . show) e
  pretty (DictionaryErrorParsecFunc e)
   = "Function parse error:" <+> (text . show) e
  pretty (DictionaryErrorParse es)
   = pretty es
  pretty (DictionaryErrorCheck e)
   = pretty e


