{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Icicle.Compiler.Source
  ( ErrorSource (..)

  , IsName

  , Var
  , TypeAnnot
  , AnnotUnit

  , QueryUntyped
  , QueryTyped

  , CoreProgramUntyped

  , Funs
  , FunEnvT

    -- * Compiler options
  , IcicleCompileOptions (..)
  , defaultCompileOptions
  , Inline.InlineOption (..)
  , Inline.defaultInline
  , Check.CheckOptions (..)
  , Check.defaultCheckOptions

    -- * From dictionaires and libraries
  , queryOfSource
  , entryOfQuery
  , readIcicleLibrary

    -- * Works on Source programs
  , sourceParseQT
  , sourceParseF
  , sourceDesugarQT
  , sourceDesugarF
  , sourceReifyQT
  , sourceCheckQT
  , sourceCheckF
  , sourceInline

    -- * Helpers
  , freshNamer
  , annotOfError
  ) where


import           Icicle.Common.Base                       (Name)
import qualified Icicle.Common.Base                       as Common
import qualified Icicle.Common.Fresh                      as Fresh

import qualified Icicle.Core.Program.Program              as Core

import           Icicle.Dictionary                        (Dictionary, DictionaryEntry(..))
import qualified Icicle.Dictionary                        as Dict

import           Icicle.Internal.Pretty

import qualified Icicle.Source.Checker                    as Check
import qualified Icicle.Source.Parser                     as Parse
import qualified Icicle.Source.Query                      as Query
import qualified Icicle.Source.Transform.Desugar          as Desugar
import qualified Icicle.Source.Transform.Inline           as Inline
import qualified Icicle.Source.Transform.ReifyPossibility as Reify
import qualified Icicle.Source.Type                       as Type

import           Icicle.Data

import           Data.Functor.Identity
import qualified Data.Map                                 as M
import           Data.Monoid
import           Data.String
import           Data.Hashable                            (Hashable)

import qualified Text.ParserCombinators.Parsec            as Parsec

import           GHC.Generics                             (Generic)

import           P

import           X.Control.Monad.Trans.Either



type IsName v = (Hashable v, Eq v, IsString v, Pretty v, Show v, NFData v)

type AnnotUnit = ()

type Var         = Parse.Variable
type TypeAnnot a = Type.Annot a Var

type Funs a b = [((a, Name b), Query.Function a b)]
type FunEnvT a b = [ ( Name b
                   , ( Type.FunctionType b
                     , Query.Function (TypeAnnot a) b )) ]

type QueryUntyped v = Query.QueryTop            Parsec.SourcePos  v
type QueryTyped   v = Query.QueryTop (TypeAnnot Parsec.SourcePos) v

type CoreProgramUntyped v = Core.Program AnnotUnit v

type Error = ErrorSource Var

freshNamer :: IsString v => v -> Fresh.NameState v
freshNamer prefix
 = Fresh.counterPrefixNameState (fromString . show) prefix

--------------------------------------------------------------------------------

data IcicleCompileOptions = IcicleCompileOptions
  { icicleInline  :: Inline.InlineOption
  , icicleBigData :: Check.CheckOptions
  }

defaultCompileOptions :: IcicleCompileOptions
defaultCompileOptions = IcicleCompileOptions Inline.defaultInline Check.defaultCheckOptions

--------------------------------------------------------------------------------

data ErrorSource var
 = ErrorSourceParse       !Parsec.ParseError
 | ErrorSourceDesugar     !(Desugar.DesugarError Parsec.SourcePos var)
 | ErrorSourceCheck       !(Check.CheckError     Parsec.SourcePos var)
 deriving (Show, Generic)

-- deepseq stops here, we don't really care about sequencing the error
-- just need this to make sure the return type (with an AST) is sequenced.
instance NFData (ErrorSource a) where rnf _ = ()

annotOfError :: ErrorSource a -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ErrorSourceParse sp
     -> Just
      $ Parsec.errorPos sp
    ErrorSourceDesugar e'
     -> Desugar.annotOfError e'
    ErrorSourceCheck       e'
     -> Check.annotOfError  e'

instance (Hashable a, Eq a, IsString a, Pretty a, Show a) => Pretty (ErrorSource a) where
 pretty e
  = case e of
     ErrorSourceParse p
      -> "Parse error:" <> line
      <> indent 2 (text $ show p)
     ErrorSourceDesugar d
      -> "Desugar error:" <> line
      <> indent 2 (pretty d)
     ErrorSourceCheck ce
      -> "Check error:" <> line
      <> indent 2 (pretty ce)

--------------------------------------------------------------------------------

-- * queries

queryOfSource :: Check.CheckOptions
              -> Dictionary
              -> Text
              -> Text
              -> Text
              -> Either Error (Attribute, QueryTyped Var)
queryOfSource checkOpts dict name src namespace = do
  parsed       <- sourceParseQT name (Namespace namespace) src
  desugared    <- sourceDesugarQT parsed
  (checked, _) <- sourceCheckQT checkOpts dict desugared
  pure (Attribute name, checked)

entryOfQuery :: Attribute
             -> QueryTyped Var
             -> Text
             -> DictionaryEntry
entryOfQuery attr query nsp
  = Dict.DictionaryEntry attr
      (Dict.VirtualDefinition (Dict.Virtual query)) (Namespace nsp)

-- * source

sourceParseQT :: Text
              -> Namespace
              -> Text
              -> Either Error (QueryUntyped Var)
sourceParseQT base namespace t
 = first ErrorSourceParse
 $ Parse.parseQueryTop (Common.OutputName base namespace) t

sourceParseF :: Parsec.SourceName
             -> Text
             -> Either Error (Funs Parsec.SourcePos Var)
sourceParseF env t
 = first ErrorSourceParse
 $ Parse.parseFunctions env t

sourceDesugarQT ::               QueryUntyped Var
                -> Either Error (QueryUntyped Var)
sourceDesugarQT q
 = runIdentity . runEitherT . bimapEitherT ErrorSourceDesugar snd
 $ Fresh.runFreshT
     (Desugar.desugarQT q)
     (freshNamer "desugar_q")

sourceDesugarF :: Funs Parsec.SourcePos Var
               -> Either (ErrorSource Var) (Funs Parsec.SourcePos Var)
sourceDesugarF fun
 = runIdentity . runEitherT . bimapEitherT ErrorSourceDesugar snd
 $ Fresh.runFreshT
     (mapM (mapM Desugar.desugarFun) fun)
     (freshNamer "desugar_f")

sourceReifyQT :: QueryTyped Var
              -> QueryTyped Var
sourceReifyQT q
 = snd
 $ runIdentity
 $ Fresh.runFreshT
     (Reify.reifyPossibilityQT q)
     (freshNamer "reify")

sourceCheckQT :: Check.CheckOptions
              -> Dictionary
              -> QueryUntyped Var
              -> Either Error (QueryTyped Var, Type.Type Var)
sourceCheckQT opts d q
 = let d' = Dict.featureMapOfDictionary d
   in  first ErrorSourceCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "check")
     $ runEitherT
     $ Check.checkQT opts d' q

sourceCheckF :: FunEnvT Parsec.SourcePos Var
             -> Funs    Parsec.SourcePos Var
             -> Either  Error (FunEnvT Parsec.SourcePos Var)
sourceCheckF env parsedImport
 = first ErrorSourceCheck
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ Check.checkFs env parsedImport

sourceInline :: Inline.InlineOption
             -> Dictionary
             -> QueryTyped   Var
             -> QueryUntyped Var
sourceInline opt d q
 = Query.reannotQT Type.annAnnot
 $ inline q
 where
  funs      = M.map snd
            $ M.fromList
            $ Dict.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (Inline.inlineQT opt funs q')
                (freshNamer "inline")

readIcicleLibrary :: Var -> Parsec.SourceName -> Text -> Either Error (FunEnvT Parsec.SourcePos Var)
readIcicleLibrary name source input
 = do input' <- first ErrorSourceParse $ Parse.parseFunctions source input
      first ErrorSourceCheck
             $ snd
             $ flip Fresh.runFresh (freshNamer name)
             $ runEitherT
             $ Check.checkFs [] input'
