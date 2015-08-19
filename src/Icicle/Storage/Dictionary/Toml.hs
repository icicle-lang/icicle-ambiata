{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , loadDictionary
  ) where

import           Icicle.Dictionary.Data
import           Icicle.Storage.Dictionary.Toml.Toml
import           Icicle.Storage.Dictionary.Toml.TomlDictionary

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh                as Fresh

import qualified Icicle.Source.Type                 as ST

import qualified Icicle.Source.Checker.Checker      as SC
import qualified Icicle.Source.Checker.Error        as SC
import qualified Icicle.Source.Checker.Function     as SC

import qualified Icicle.Source.Parser               as SP

import           Icicle.Internal.Pretty hiding ((</>))

import           P

import qualified Control.Arrow                      as A
import           Control.Monad.Trans.Either
import qualified Control.Exception                  as E

import           System.IO
import           System.FilePath

import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T

import qualified Data.Map                           as M

import qualified Text.Parsec                        as Parsec


data DictionaryImportError
  = DictionaryErrorIO E.SomeException
  | DictionaryErrorParsecTOML Parsec.ParseError
  | DictionaryErrorParsecFunc Parsec.ParseError
  | DictionaryErrorParse [DictionaryValidationError]
  | DictionaryErrorCheck (SC.CheckError Parsec.SourcePos SP.Variable)
  deriving (Show)

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary dictionary
 = loadDictionary' M.empty mempty [] dictionary

loadDictionary'
  :: M.Map (Name SP.Variable) (ST.FunctionType SP.Variable)
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

  parsedImports
    <- (\fp' ->
         EitherT
         $ ((A.left DictionaryErrorParsecFunc) . SP.parseFunctions)
        <$> T.readFile (rp </> (T.unpack fp'))
       ) `traverse` (imports conf)

  importedFunctions
    <- hoistEither $ (A.left DictionaryErrorCheck)
     $ foldlM
     ( \(env, _) -> \parsedImport ->
       snd
       $ flip Fresh.runFresh (freshNamer "f")
       $ runEitherT
       $ SC.checkFs env parsedImport
     ) (parentFuncs, []) parsedImports
  let importedFunctions'
        = M.intersectionWith (,)
                       (fst $ importedFunctions)
          (M.fromList $ snd $ importedFunctions)

  let concreteDefinitions = foldr remakeConcrete [] definitions'
  let virtualDefinitions' = foldr remakeVirtuals [] definitions'

  let d' = featureMapOfDictionary $ Dictionary (concreteDefinitions <> parentConcrete) importedFunctions'

  virtualDefinitions
    <- flip traverse virtualDefinitions'
     $ \(a, q) ->
       (\(q', _) -> DictionaryEntry a (VirtualDefinition (Virtual q')))
      <$>
       ( hoistEither
       $ (A.left DictionaryErrorCheck)
       $ snd
       $ flip Fresh.runFresh (freshNamer "t")
       $ runEitherT
       $ SC.checkQT d' q
       )

  loadedChapters
    <- (\fp' ->
         loadDictionary' (fst importedFunctions) conf concreteDefinitions (rp </> (T.unpack fp'))
       ) `traverse` (chapter conf)

  let functions = M.unions $ importedFunctions' : (dictionaryFunctions <$> loadedChapters)
  let totaldefinitions = concreteDefinitions <> virtualDefinitions <> (join $ dictionaryEntries <$> loadedChapters)

  pure $ Dictionary totaldefinitions functions
    where
      rp = (takeDirectory fp)

      remakeConcrete (DictionaryEntry' a (ConcreteDefinition' e)) cds = (DictionaryEntry a (ConcreteDefinition e)) : cds
      remakeConcrete _ cds = cds

      remakeVirtuals (DictionaryEntry' a (VirtualDefinition' (Virtual' v))) vds = (a, v) : vds
      remakeVirtuals _ vds = vds


-- TODO, move this somewhere sensible.
freshNamer :: T.Text -> Fresh.NameState SP.Variable
freshNamer prefix = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) (SP.Variable prefix)


instance Pretty DictionaryImportError where
  pretty (DictionaryErrorIO e)
   = "IO Exception:" <+> (text . show) e
  pretty (DictionaryErrorParsecTOML e)
   = "TOML parse error:" <+> (text . show) e
  pretty (DictionaryErrorParsecFunc e)
   = "Function parse error:" <+> (text . show) e
  pretty (DictionaryErrorParse es)
   = vcat $ fmap pretty es
  pretty (DictionaryErrorCheck e)
   = pretty e

