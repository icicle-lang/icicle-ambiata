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
import qualified Icicle.Source.ToCore.Context       as SC

import qualified Icicle.Source.Query                as SQ
import qualified Icicle.Source.Parser               as SP
import qualified Icicle.Source.Transform.Inline     as STI

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
import qualified Data.Set                           as S

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

  parsedImports
    <- (\fp' -> do
         let fp'' = T.unpack fp'
         importsText
           <- EitherT
            $ A.left DictionaryErrorIO
           <$> E.try (T.readFile (rp </> fp''))
         hoistEither
            $ A.left DictionaryErrorParsecFunc
            $ SP.parseFunctions fp'' importsText
       ) `traverse` (imports conf)

  importedFunctions
    <- hoistEither $ (A.left DictionaryErrorCheck)
     $ foldlM
     ( \env -> \parsedImport ->
       snd
       $ flip Fresh.runFresh (freshNamer "f")
       $ runEitherT
       $ SC.checkFs env parsedImport
     ) parentFuncs parsedImports

  let concreteDefinitions = foldr remakeConcrete [] definitions'
  let virtualDefinitions' = foldr remakeVirtuals [] definitions'

  let d' = Dictionary (concreteDefinitions <> parentConcrete) importedFunctions
  let d'' = featureMapOfDictionary $ d'

  virtualDefinitions
    <- (\(a, q) -> do
         (checked, _)  <- sourceCheck d'' q
         let inlined    = sourceInline d' checked
         (checked', _) <- sourceCheck d'' inlined
         pure $ DictionaryEntry a (VirtualDefinition (Virtual checked'))
       ) `traverse` virtualDefinitions'

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

sourceCheck :: Monad m
            => SC.Features () SP.Variable
            -> SQ.QueryTop Parsec.SourcePos SP.Variable
            -> EitherT DictionaryImportError m (SQ.QueryTop (ST.Annot Parsec.SourcePos SP.Variable) SP.Variable, ST.Type SP.Variable)
sourceCheck d q =
  hoistEither
       $ (A.left DictionaryErrorCheck)
       $ snd
       $ flip Fresh.runFresh (freshNamer "t")
       $ runEitherT
       $ SC.checkQT d q

sourceInline :: Dictionary
             -> SQ.QueryTop (ST.Annot Parsec.SourcePos SP.Variable) SP.Variable
             -> SQ.QueryTop Parsec.SourcePos SP.Variable
sourceInline d q
 = SQ.reannotQT ST.annAnnot
 $ inline q
 where
  funs      = M.map snd
            $ dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (STI.inlineQT funs q')
                (freshNamer "inline")


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

