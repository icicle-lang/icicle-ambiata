{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary
  , loadDenseDictionary
  ) where

import           Icicle.Common.Base

import           Icicle.Data                                   (Attribute, Namespace(..))
import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import qualified Icicle.Compiler.Source                        as P

import qualified Icicle.Source.Checker                         as SC
import qualified Icicle.Source.Parser                          as SP
import qualified Icicle.Source.Query                           as SQ
import qualified Icicle.Source.Type                            as ST

import           Icicle.Storage.Dictionary.Toml.Dense
import           Icicle.Storage.Dictionary.Toml.Toml
import           Icicle.Storage.Dictionary.Toml.TomlDictionary
import           Icicle.Storage.Dictionary.Toml.Types

import qualified Control.Exception                             as E

import           Data.FileEmbed (embedFile)

import           System.FilePath
import           System.IO

import qualified Data.Set                                      as Set
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Text.IO                                  as T

import qualified Text.Parsec                                   as Parsec

import           P

import           X.Control.Monad.Trans.Either


data DictionaryImportError
  = DictionaryErrorIO          E.SomeException
  | DictionaryErrorParsecTOML  Parsec.ParseError
  | DictionaryErrorCompilation (P.ErrorSource P.Var)
  | DictionaryErrorParse       [DictionaryValidationError]
  | DictionaryErrorDense       DictionaryDenseError
  deriving (Show)

type Funs a  = [((a, Name SP.Variable), SQ.Function a SP.Variable)]
type FunEnvT = [ ( Name SP.Variable
                 , ( ST.FunctionType SP.Variable
                   , SQ.Function (ST.Annot Parsec.SourcePos SP.Variable) SP.Variable ) ) ]

data ImplicitPrelude = ImplicitPrelude | NoImplicitPrelude
  deriving (Eq, Ord, Show)

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: SC.CheckOptions -> ImplicitPrelude -> FilePath -> EitherT DictionaryImportError IO Dictionary
loadDictionary checkOpts impPrelude dictionary
 = loadDictionary' checkOpts impPrelude [] mempty [] dictionary

loadDenseDictionary
  :: SC.CheckOptions
  -> ImplicitPrelude
  -> FilePath
  -> Maybe PsvInputDenseFeedName
  -> EitherT DictionaryImportError IO (Dictionary, PsvInputDenseDict)
loadDenseDictionary checkOpts impPrelude dictionary feed
  = do d    <- loadDictionary checkOpts impPrelude dictionary
       toml <- parseTOML dictionary
       dd   <- firstEitherT DictionaryErrorDense $ hoistEither $ denseFeeds d toml feed
       return (d, dd)

loadDictionary'
  :: SC.CheckOptions
  -> ImplicitPrelude
  -> FunEnvT
  -> DictionaryConfig
  -> [DictionaryEntry]
  -> FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary' checkOpts impPrelude parentFuncs parentConf parentConcrete dictPath = do
  rawToml              <- parseTOML dictPath
  (conf, definitions') <- firstEitherT DictionaryErrorParse . hoistEither . toEither
                        $ tomlDict parentConf rawToml

  let repoPath = takeDirectory dictPath

  rawImports        <- traverse (readImport repoPath) (fmap T.unpack (imports conf))
  let prelude'      =  if impPrelude == ImplicitPrelude then prelude else []
  parsedImports     <- hoistEither $ traverse (uncurry parseImport) (prelude' <> rawImports)
  importedFunctions <- loadImports parentFuncs parsedImports

  -- Functions available for virtual features, and visible in sub-dictionaries.
  let availableFunctions = parentFuncs <> importedFunctions

  let concreteDefinitions = foldr remakeConcrete [] definitions'
  let virtualDefinitions' = foldr remakeVirtuals [] definitions'

  let d' = Dictionary (concreteDefinitions <> parentConcrete) availableFunctions

  virtualDefinitions <- checkDefs checkOpts d' virtualDefinitions'

  let loadChapter fp' = loadDictionary' checkOpts
                                        NoImplicitPrelude
                                        availableFunctions
                                        conf
                                        concreteDefinitions
                                        (repoPath </> T.unpack fp')

  loadedChapters <- traverse loadChapter (chapter conf)

  -- Dictionaries loaded after one another can see the functions of previous
  -- dictionaries. So sub-dictionaries imports can use prelude functions.
  -- Export the dictionaries loaded here, and in sub dictionaries (but not
  -- parent functions, as the parent already knows about those).
  let functions = join $ [importedFunctions] <> (dictionaryFunctions <$> loadedChapters)
  let totaldefinitions = concreteDefinitions <> virtualDefinitions <> (join $ dictionaryEntries <$> loadedChapters)

  pure $ Dictionary totaldefinitions functions

parseTOML :: FilePath -> EitherT DictionaryImportError IO Table
parseTOML dictPath = do
  inputText <- firstEitherT DictionaryErrorIO . EitherT $ E.try (readFile dictPath)
  rawToml   <- firstEitherT DictionaryErrorParsecTOML . hoistEither $ Parsec.parse tomlDoc dictPath inputText
  return rawToml

remakeConcrete
  :: DictionaryEntry'
  -> [DictionaryEntry]
  -> [DictionaryEntry]
remakeConcrete de cds
 = case de of
    DictionaryEntry' a (ConcreteDefinition' e t ty) nsp
     -> DictionaryEntry a (ConcreteDefinition e (Set.fromList (toList t)) ty) nsp
      : cds
    _
     -> cds

remakeVirtuals
  :: DictionaryEntry'
  -> [(Namespace, Attribute, SQ.QueryTop Parsec.SourcePos SP.Variable)]
  -> [(Namespace, Attribute, SQ.QueryTop Parsec.SourcePos SP.Variable)]
remakeVirtuals de vds
 = case de of
     DictionaryEntry' a (VirtualDefinition' (Virtual' v)) nsp
      -> (nsp, a, v) : vds
     _
      -> vds

readImport :: FilePath -> FilePath -> EitherT DictionaryImportError IO (FilePath, Text)
readImport repoPath fileRel
 = firstEitherT DictionaryErrorIO . EitherT . E.try $ do
     let fileAbs = repoPath </> fileRel
     src <- T.readFile fileAbs
     return (fileRel, src)

parseImport :: FilePath -> Text -> Either DictionaryImportError (Funs Parsec.SourcePos)
parseImport path src
 = first DictionaryErrorCompilation (P.sourceParseF path src)

loadImports :: FunEnvT -> [Funs Parsec.SourcePos] -> EitherT DictionaryImportError IO FunEnvT
loadImports parentFuncs parsedImports
 = hoistEither . first DictionaryErrorCompilation
 $ foldlM (go parentFuncs) [] parsedImports
 where
  go env acc f
   = do -- Run desugar to ensure pattern matches are complete.
        _  <- P.sourceDesugarF f
        -- Type check the function (allowing it to use parents and previous).
        f' <- P.sourceCheckF (env <> acc) f
        -- Return these functions at the end of the accumulator.
        return $ acc <> f'

checkDefs
  :: SC.CheckOptions
  -> Dictionary
  -> [(Namespace, Attribute, P.QueryUntyped P.Var)]
  -> EitherT DictionaryImportError IO [DictionaryEntry]
checkDefs checkOpts d defs
 = hoistEither . first DictionaryErrorCompilation
 $ go `traverse` defs
 where
  go (n, a, q)
   = do  -- Run desugar to ensure pattern matches are complete.
         _             <- P.sourceDesugarQT q
         -- Type check the virtual definition.
         (checked, _)  <- P.sourceCheckQT checkOpts d q
         pure $ DictionaryEntry a (VirtualDefinition (Virtual checked)) n



instance Pretty DictionaryImportError where
  pretty = \case
    DictionaryErrorIO          e  -> "IO Exception:" <+> (text . show) e
    DictionaryErrorParsecTOML  e  -> "TOML parse error:" <+> (text . show) e
    DictionaryErrorCompilation e  -> pretty e
    DictionaryErrorParse       es -> "Validation error:" <+> align (vcat (pretty <$> es))
    DictionaryErrorDense       e  -> "Parse dense feeds error:" <+> (text . show) e

------------------------------------------------------------------------

prelude :: [(FilePath, Text)]
prelude
 = [("prelude.icicle", T.decodeUtf8 $(embedFile "data/libs/prelude.icicle"))]

