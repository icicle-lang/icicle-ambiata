{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE PatternGuards     #-}

module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary
  , loadDenseDictionary
  , prelude
  ) where

import           Icicle.Common.Base

import           Icicle.Data                                   (Attribute(..), Namespace(..))
import           Icicle.Dictionary.Data

import           Icicle.Internal.Pretty                        hiding ((</>))

import qualified Icicle.Compiler.Source                        as P

import           Icicle.Source.Checker                         (CheckOptions (..))
import qualified Icicle.Source.Parser                          as SP
import           Icicle.Source.Query                           (QueryTop (..), Query (..), Exp)
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

import           Text.Parsec                                   (SourcePos)
import qualified Text.Parsec                                   as Parsec
import qualified Text.Parsec.Pos                               as Parsec

import           P

import           X.Control.Monad.Trans.Either


data DictionaryImportError
  = DictionaryErrorIO          E.SomeException
  | DictionaryErrorParsecTOML  Parsec.ParseError
  | DictionaryErrorCompilation (P.ErrorSource P.Var)
  | DictionaryErrorParse       [DictionaryValidationError]
  | DictionaryErrorDense       DictionaryDenseError
  | DictionaryErrorImpossible
  deriving (Show)

type Funs a  = [((a, Name SP.Variable), SQ.Function a SP.Variable)]
type FunEnvT = [ ( Name SP.Variable
                 , ( ST.FunctionType SP.Variable
                   , SQ.Function (ST.Annot Parsec.SourcePos SP.Variable) SP.Variable ) ) ]

data ImplicitPrelude = ImplicitPrelude | NoImplicitPrelude
  deriving (Eq, Ord, Show)

-- Top level IO function which loads all dictionaries and imports
loadDictionary :: CheckOptions -> ImplicitPrelude -> FilePath -> EitherT DictionaryImportError IO Dictionary
loadDictionary checkOpts impPrelude dictionary
 = loadDictionary' checkOpts impPrelude [] mempty [] dictionary

loadDenseDictionary
  :: CheckOptions
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
  :: CheckOptions
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

  -- Do a convoluted dance to construct the concrete definitions without the keys
  -- so that we can check the keys before making the actual concrete definitions.
  let virtualDefinitions'  = foldr remakeVirtuals [] definitions'
  let concreteDefinitions' = foldr remakeConcrete [] definitions'
  let dictUnchecked        = Dictionary (fmap snd concreteDefinitions' <> parentConcrete) availableFunctions

  virtualDefinitions  <- checkDefs checkOpts dictUnchecked virtualDefinitions'
  concreteDefinitions <- hoistEither
                       $ forM concreteDefinitions'
                       $ \(ConcreteKey' k, e@(DictionaryEntry a def n)) -> case k of
                           Nothing
                            -> pure e
                           Just key
                            | ConcreteDefinition en ts _ <- def
                            -> do k' <- checkKey checkOpts dictUnchecked a n key
                                  pure $ DictionaryEntry a (ConcreteDefinition en ts k') n
                            | otherwise
                            -> pure e

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
  where
    remakeConcrete (DictionaryEntry' a (ConcreteDefinition' e t k) nsp) cds
      = (k, DictionaryEntry a (ConcreteDefinition e (Set.fromList (toList t)) unkeyed) nsp)
      : cds
    remakeConcrete _ cds
      = cds

    remakeVirtuals de vds
     = case de of
         DictionaryEntry' a (VirtualDefinition' (Virtual' v)) nsp
          -> (nsp, a, v) : vds
         _
          -> vds


parseTOML :: FilePath -> EitherT DictionaryImportError IO Table
parseTOML dictPath = do
  inputText <- firstEitherT DictionaryErrorIO . EitherT $ E.try (readFile dictPath)
  rawToml   <- firstEitherT DictionaryErrorParsecTOML . hoistEither $ Parsec.parse tomlDoc dictPath inputText
  return rawToml

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

checkDefs :: CheckOptions
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

checkKey :: CheckOptions
         -> Dictionary
         -> Attribute
         -> Namespace
         -> Exp SourcePos P.Var
         -> Either DictionaryImportError (ConcreteKey AnnotSource P.Var)
checkKey checkOpts d attr nsp xx = do
  let l = Parsec.initialPos "dummy_pos_ctx"
  let p = Parsec.initialPos "dummy_pos_final"
  let q = QueryTop (nameOf (NameBase (SP.Variable (getAttribute attr))))
                   (OutputName "dummy_output" nsp)
          -- We know the key must be of Pure or Element temporality,
          -- so it's ok to wrap it in a Group.
          (Query   [SQ.Distinct l xx]
          -- The final expression just needs to be Aggregate.
                   (SQ.Prim p (SQ.Lit (SQ.LitInt 0))))

  (checked, _)  <- first DictionaryErrorCompilation $ do
    q'       <- P.sourceDesugarQT q
    (q'', t) <- P.sourceCheckQT checkOpts d q'
    return (P.sourceReifyQT q'', t)

  case contexts . query $ checked of
    SQ.Distinct _ xx' : _
      -> Right . ConcreteKey . Just $ xx'
    _ -> Left DictionaryErrorImpossible

instance Pretty DictionaryImportError where
  pretty = \case
    DictionaryErrorIO          e  -> "IO Exception:" <+> (text . show) e
    DictionaryErrorParsecTOML  e  -> "TOML parse error:" <+> (text . show) e
    DictionaryErrorCompilation e  -> pretty e
    DictionaryErrorParse       es -> "Validation error:" <+> align (vcat (pretty <$> es))
    DictionaryErrorDense       e  -> "Parse dense feeds error:" <+> (text . show) e
    DictionaryErrorImpossible     -> "Impossible!"

------------------------------------------------------------------------

prelude :: [(FilePath, Text)]
prelude
 = [("prelude.icicle", T.decodeUtf8 $(embedFile "data/libs/prelude.icicle"))]
