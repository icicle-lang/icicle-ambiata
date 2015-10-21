{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Pipeline
  ( CompileError(..)
  , QueryTop', QueryTop'T, Program', ProgramT
  , annotOfError
  , sourceParseQT
  , sourceParseF
  , sourceDesugarQT
  , sourceDesugarF
  , sourceReifyQT
  , sourceCheckQT
  , sourceCheckF
  , sourceConvert
  , sourceInline
  , coreSimp
  , coreFlatten
  , checkAvalanche
  , coreAvalanche
  , simpAvalanche
  , simpFlattened
  , freshNamer
  ) where

import qualified Icicle.Avalanche.FromCore                as AC
import qualified Icicle.Avalanche.Check                   as AC
import qualified Icicle.Avalanche.Prim.Flat               as APF
import qualified Icicle.Avalanche.Program                 as AP
import qualified Icicle.Avalanche.Simp                    as AS
import qualified Icicle.Avalanche.Statement.Flatten       as AS
import           Icicle.Common.Base                       (Name)
import qualified Icicle.Common.Base                       as CommonBase
import qualified Icicle.Common.Annot                      as CommonAnnotation
import qualified Icicle.Common.Fresh                      as Fresh
import qualified Icicle.Core.Program.Condense             as Core
import qualified Icicle.Core.Program.Program              as Core
import qualified Icicle.Core.Program.Simp                 as Core
import qualified Icicle.Core.Exp.Prim                     as Core

import qualified Icicle.Dictionary                        as D
import           Icicle.Internal.Pretty
import qualified Icicle.Source.Checker                    as SC
import qualified Icicle.Source.Parser                     as SP
import qualified Icicle.Source.Query                      as SQ
import qualified Icicle.Source.ToCore.Base                as STC
import qualified Icicle.Source.ToCore.ToCore              as STC
import qualified Icicle.Source.Transform.Desugar          as STD
import qualified Icicle.Source.Transform.Inline           as STI
import qualified Icicle.Source.Transform.ReifyPossibility as STR
import qualified Icicle.Source.Type                       as ST

import           Control.Monad.Trans.Either

import           Data.Either.Combinators
import           Data.Functor.Identity
import qualified Data.Map                                 as M
import           Data.Monoid
import           Data.Text                                (Text)
import qualified Data.Text                                as T


import qualified Text.ParserCombinators.Parsec            as Parsec

import           P

--------------------------------------------------------------------------------

data CompileError a
 = CompileErrorParse   Parsec.ParseError
 | CompileErrorDesugar (STD.DesugarError a Var)
 | CompileErrorCheck   (SC.CheckError a Var)
 | CompileErrorConvert (STC.ConvertError a Var)
 | CompileErrorFlatten (AS.FlattenError () Text)
 | CompileErrorProgram (AC.ProgramError () Text APF.Prim)
 deriving (Show)


annotOfError :: CompileError Parsec.SourcePos -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    CompileErrorParse sp
     -> Just
      $ Parsec.errorPos sp
    CompileErrorDesugar e'
     -> STD.annotOfError e'
    CompileErrorCheck       e'
     -> SC.annotOfError  e'
    CompileErrorConvert     e'
     -> STC.annotOfError e'
    CompileErrorFlatten _
     -> Nothing
    CompileErrorProgram _
     -> Nothing

instance Pretty a => Pretty (CompileError a) where
 pretty e
  = case e of
     CompileErrorParse p
      -> "Parse error:" <> line
      <> indent 2 (text $ show p)
     CompileErrorDesugar d
      -> "Desugar error:" <> line
      <> indent 2 (pretty d)
     CompileErrorCheck ce
      -> "Check error:" <> line
      <> indent 2 (pretty ce)
     CompileErrorConvert ce
      -> "Convert error:" <> line
      <> indent 2 (pretty ce)
     CompileErrorFlatten d
      -> "Flatten error:" <> line
      <> indent 2 (text $ show d)
     CompileErrorProgram d
      -> "Program error:" <> line
      <> indent 2 (text $ show d)

--------------------------------------------------------------------------------

-- * Compile

type Var        = SP.Variable
type AnnotT     = ST.Annot Parsec.SourcePos Var

type Program'   = Core.Program () Var
type ProgramT   = Core.Program () Text

type QueryTop'  = SQ.QueryTop Parsec.SourcePos Var
type QueryTop'T = SQ.QueryTop AnnotT Var

type Funs a  = [((a, Name SP.Variable), SQ.Function a SP.Variable)]
type FunEnvT = [ ( Name Var
                 , ( ST.FunctionType SP.Variable
                   , SQ.Function AnnotT SP.Variable ) ) ]


sourceParseQT :: Text -> Text -> Either (CompileError Parsec.SourcePos) QueryTop'
sourceParseQT base t
 = mapLeft CompileErrorParse
 $ SP.parseQueryTop (CommonBase.OutputName base) t

sourceParseF :: Parsec.SourceName -> Text -> Either (CompileError Parsec.SourcePos) (Funs Parsec.SourcePos)
sourceParseF env t
 = mapLeft CompileErrorParse
 $ SP.parseFunctions env t


sourceDesugarQT :: QueryTop' -> Either (CompileError Parsec.SourcePos) QueryTop'
sourceDesugarQT q
 = runIdentity . runEitherT . bimapEitherT CompileErrorDesugar snd
 $ Fresh.runFreshT
     (STD.desugarQT q)
     (freshNamer "desugar_q")

sourceDesugarF :: Funs Parsec.SourcePos -> Either (CompileError Parsec.SourcePos) (Funs Parsec.SourcePos)
sourceDesugarF fun
 = runIdentity . runEitherT . bimapEitherT CompileErrorDesugar snd
 $ Fresh.runFreshT
     (mapM (mapM STD.desugarFun) fun)
     (freshNamer "desugar_f")


sourceReifyQT :: QueryTop'T -> QueryTop'T
sourceReifyQT q
 = snd
 $ runIdentity
 $ Fresh.runFreshT
     (STR.reifyPossibilityQT q)
     (freshNamer "reify")


sourceCheckQT :: D.Dictionary -> QueryTop' -> Either (CompileError Parsec.SourcePos) (QueryTop'T, ST.Type Var)
sourceCheckQT d q
 = let d' = D.featureMapOfDictionary d
   in  mapLeft CompileErrorCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "check")
     $ runEitherT
     $ SC.checkQT d' q

sourceCheckF :: FunEnvT -> Funs Parsec.SourcePos -> Either (CompileError Parsec.SourcePos) FunEnvT
sourceCheckF env parsedImport
 = mapLeft CompileErrorCheck
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ SC.checkFs env parsedImport


sourceConvert :: D.Dictionary -> QueryTop'T -> Either (CompileError Parsec.SourcePos) Program'
sourceConvert d q
 = mapRight snd
 $ mapLeft CompileErrorConvert conv
 where
  d'        = D.featureMapOfDictionary d
  conv      = Fresh.runFreshT
                (STC.convertQueryTop d' q)
                (freshNamer "conv")


sourceInline :: D.Dictionary -> QueryTop'T -> QueryTop'
sourceInline d q
 = SQ.reannotQT ST.annAnnot
 $ inline q
 where
  funs      = M.map snd
            $ M.fromList
            $ D.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (STI.inlineQT funs q')
                (freshNamer "inline")

coreFlatten :: ProgramT -> Either (CompileError a) (AP.Program () Text APF.Prim)
coreFlatten prog
 = let av = coreAvalanche prog
       ns = Fresh.counterPrefixNameState (T.pack . show) "flat"
   in   mapLeft  CompileErrorFlatten
      . mapRight simpFlattened
      . mapRight (\(_,s') -> av { AP.statements = s' })
      $ Fresh.runFreshT (AS.flatten () $ AP.statements av) ns

checkAvalanche :: AP.Program () Text APF.Prim
               -> Either (CompileError a) (AP.Program (CommonAnnotation.Annot ()) Text APF.Prim)
checkAvalanche prog
 = mapLeft CompileErrorProgram
 $ AC.checkProgram APF.flatFragment prog

coreAvalanche :: ProgramT -> AP.Program () Text Core.Prim
coreAvalanche prog
 = simpAvalanche
 $ AC.programFromCore (AC.namerText id) prog

simpAvalanche :: (Eq p, Show p) => AP.Program () Text p -> AP.Program () Text p
simpAvalanche av
 = let simp = AS.simpAvalanche () av
       name = Fresh.counterPrefixNameState (T.pack . show) "anf"
   in  snd $ Fresh.runFresh simp name

simpFlattened :: AP.Program () Text APF.Prim -> AP.Program () Text APF.Prim
simpFlattened av
 = let simp = AS.simpFlattened () av
       name = Fresh.counterPrefixNameState (T.pack . show) "simp"
   in  snd $ Fresh.runFresh (simp >>= AS.simpFlattened ()) name


coreSimp :: Program' -> Program'
coreSimp p
 = Core.condenseProgram ()
 $ snd
 $ Fresh.runFresh (Core.simpProgram () p) (freshNamer "simp")


freshNamer :: Text -> Fresh.NameState SP.Variable
freshNamer prefix = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) (SP.Variable prefix)
