{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Storage.Dictionary.Toml (
    DictionaryImportError (..)
  , ImplicitPrelude (..)
  , loadDictionary
  , loadDenseDictionary
  , prelude
  , fromFunEnv
  , toFunEnv
  ) where

import           Icicle.Common.Base

import           Icicle.Data
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
import qualified Data.Map.Strict                               as Map

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
  -> [DictionaryFunction]
  -> DictionaryConfig
  -> [DictionaryInput]
  -> FilePath
  -> EitherT DictionaryImportError IO Dictionary
loadDictionary' checkOpts impPrelude parentFuncs parentConf parentInputs dictPath = do
  rawToml              <- parseTOML dictPath
  (conf, inputs0', outputs0') <- firstEitherT DictionaryErrorParse . hoistEither . toEither
                        $ tomlDict parentConf rawToml

  let repoPath = takeDirectory dictPath

  rawImports        <- traverse (readImport repoPath) (fmap T.unpack (configImports conf))
  let prelude'      =  if impPrelude == ImplicitPrelude then prelude else []
  parsedImports     <- hoistEither $ traverse (uncurry parseImport) (prelude' <> rawImports)
  importedFunctions <- loadImports parentFuncs parsedImports

  -- Functions available for virtual features, and visible in sub-dictionaries.
  let availableFunctions = parentFuncs <> importedFunctions

  -- Do a convoluted dance to construct the concrete definitions without the keys
  -- so that we can check the keys before making the actual concrete definitions.
  let outputs' = foldr remakeVirtuals [] outputs0'
  let inputs' = foldr remakeConcrete [] inputs0'
  let dictUnchecked = Dictionary (mapOfInputs $ fmap snd inputs' <> parentInputs) Map.empty availableFunctions

  outputs <- checkDefs checkOpts dictUnchecked outputs'
  inputs <-
    hoistEither . forM inputs' $ \(ConcreteKey' k, e@(DictionaryInput a en ts _)) ->
      case k of
       Nothing ->
         pure e
       Just key -> do
         k' <- checkKey checkOpts dictUnchecked a key
         pure $ DictionaryInput a en ts k'

  let loadChapter fp' = loadDictionary' checkOpts
                                        NoImplicitPrelude
                                        availableFunctions
                                        conf
                                        inputs
                                        (repoPath </> T.unpack fp')

  loadedChapters <- traverse loadChapter (configChapter conf)

  -- Dictionaries loaded after one another can see the functions of previous
  -- dictionaries. So sub-dictionaries imports can use prelude functions.
  -- Export the dictionaries loaded here, and in sub dictionaries (but not
  -- parent functions, as the parent already knows about those).
  let functions = join $ [importedFunctions] <> (dictionaryFunctions <$> loadedChapters)
  let totalinputs = Map.unions $ mapOfInputs inputs : fmap dictionaryInputs loadedChapters
  let totaldefinitions = Map.unions $ mapOfOutputs outputs : fmap dictionaryOutputs loadedChapters

  pure $ Dictionary totalinputs totaldefinitions functions
  where
    remakeConcrete (DictionaryInput' a e t k) cds
      = (k, DictionaryInput a e (Set.fromList (toList t)) unkeyed)
      : cds

    remakeVirtuals (DictionaryOutput' a v) vds
     = (a, v) : vds


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

loadImports :: [DictionaryFunction] -> [Funs Parsec.SourcePos] -> EitherT DictionaryImportError IO [DictionaryFunction]
loadImports parentFuncs parsedImports
 = hoistEither . first DictionaryErrorCompilation
 $ fmap fromFunEnv
 $ foldlM (go (toFunEnv parentFuncs)) [] parsedImports
 where
  go env acc f
   = do -- Run desugar to ensure pattern matches are complete.
        _  <- P.sourceDesugarF f
        -- Type check the function (allowing it to use parents and previous).
        f' <- P.sourceCheckF (env <> acc) f
        -- Return these functions at the end of the accumulator.
        return $ acc <> f'

toFunEnv :: [DictionaryFunction] -> FunEnvT
toFunEnv =
  fmap $ \(DictionaryFunction n t f) ->
    (n, (t, f))

fromFunEnv :: FunEnvT -> [DictionaryFunction]
fromFunEnv =
  fmap $ \(n, (t, f)) ->
    DictionaryFunction n t f

checkDefs :: CheckOptions
          -> Dictionary
          -> [(OutputId, QueryTop SourcePos SP.Variable)]
          -> EitherT DictionaryImportError IO [DictionaryOutput]
checkDefs checkOpts d defs
 = hoistEither . first DictionaryErrorCompilation
 $ go `traverse` defs
 where
  go (oid, q)
   = do  -- Run desugar to ensure pattern matches are complete.
         _             <- P.sourceDesugarQT q
         -- Type check the virtual definition.
         (checked, _)  <- P.sourceCheckQT checkOpts d q
         pure $ DictionaryOutput oid checked

checkKey :: CheckOptions
         -> Dictionary
         -> InputId
         -> Exp SourcePos P.Var
         -> Either DictionaryImportError (InputKey AnnotSource P.Var)
checkKey checkOpts d iid xx = do
  let l = Parsec.initialPos "dummy_pos_ctx"
  let p = Parsec.initialPos "dummy_pos_final"
  let q = QueryTop (QualifiedInput iid) [outputid|dummy_namespace:dummy_output|]
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
      -> Right . InputKey . Just $ xx'
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
