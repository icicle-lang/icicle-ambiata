{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Pipeline
  ( CompileError(..)
  , QueryTop', QueryTop'T, Program'
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
import qualified Icicle.Avalanche.Annot                   as AA
import qualified Icicle.Avalanche.Statement.Flatten       as AS
import           Icicle.Common.Base                       (Name)
import qualified Icicle.Common.Base                       as CommonBase
import qualified Icicle.Common.Annot                      as CA
import qualified Icicle.Common.Type                       as CT
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
import           Data.String


import qualified Text.ParserCombinators.Parsec            as Parsec

import           P

--------------------------------------------------------------------------------

data CompileError a b c
 = CompileErrorParse   Parsec.ParseError
 | CompileErrorDesugar (STD.DesugarError a b)
 | CompileErrorCheck   (SC.CheckError a b)
 | CompileErrorConvert (STC.ConvertError a b)
 | CompileErrorFlatten (AS.FlattenError a b)
 | CompileErrorProgram (AC.ProgramError a b c)
 deriving (Show)


annotOfError :: CompileError Parsec.SourcePos b c -> Maybe Parsec.SourcePos
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

instance (IsString b, Ord b, Pretty a, Pretty b, Show a, Show b, Show c) => Pretty (CompileError a b c) where
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
type AnnotT a   = ST.Annot a Var

type Program'   = Core.Program () Var

type QueryTop'  = SQ.QueryTop Parsec.SourcePos Var
type QueryTop'T = SQ.QueryTop (AnnotT Parsec.SourcePos) Var

type Funs a b = [((a, Name b), SQ.Function a b)]
type FunEnvT a b = [ ( Name b
                   , ( ST.FunctionType b
                     , SQ.Function (AnnotT a) b )) ]


sourceParseQT :: Text -> Text -> Either (CompileError Parsec.SourcePos Var ()) QueryTop'
sourceParseQT base t
 = mapLeft CompileErrorParse
 $ SP.parseQueryTop (CommonBase.OutputName base) t

sourceParseF :: Parsec.SourceName -> Text -> Either (CompileError Parsec.SourcePos Var ()) (Funs Parsec.SourcePos Var)
sourceParseF env t
 = mapLeft CompileErrorParse
 $ SP.parseFunctions env t


sourceDesugarQT :: QueryTop' -> Either (CompileError Parsec.SourcePos Var ()) QueryTop'
sourceDesugarQT q
 = runIdentity . runEitherT . bimapEitherT CompileErrorDesugar snd
 $ Fresh.runFreshT
     (STD.desugarQT q)
     (freshNamer "desugar_q")

sourceDesugarF :: Funs a Var -> Either (CompileError a Var ()) (Funs a Var)
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


sourceCheckQT :: D.Dictionary -> QueryTop' -> Either (CompileError Parsec.SourcePos Var ()) (QueryTop'T, ST.Type Var)
sourceCheckQT d q
 = let d' = D.featureMapOfDictionary d
   in  mapLeft CompileErrorCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "check")
     $ runEitherT
     $ SC.checkQT d' q

sourceCheckF :: FunEnvT a Var -> Funs a Var -> Either (CompileError a Var ()) (FunEnvT a Var)
sourceCheckF env parsedImport
 = mapLeft CompileErrorCheck
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ SC.checkFs env parsedImport


sourceConvert :: D.Dictionary -> QueryTop'T -> Either (CompileError Parsec.SourcePos Var ()) Program'
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

coreFlatten :: Program' -> Either (CompileError () Var APF.Prim) (AP.Program () Var APF.Prim)
coreFlatten prog
 = let  ns = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) "flat"
   in   mapRight simpFlattened
      . join
      . mapRight snd
      . mapLeft CompileErrorFlatten
      $ Fresh.runFreshT go ns
 where
  av = coreAvalanche prog
  go
   = do s' <- AS.flatten () (AP.statements av)
        return $ checkAvalanche (av { AP.statements = s' })

checkAvalanche
  :: AP.Program () Var APF.Prim
  -> Either (CompileError () Var APF.Prim) (AP.Program (CA.Annot ()) Var APF.Prim)
checkAvalanche prog
 = mapLeft CompileErrorProgram
 $ AC.checkProgram APF.flatFragment prog

coreAvalanche :: Program' -> AP.Program () Var Core.Prim
coreAvalanche prog
 = simpAvalanche
 $ AC.programFromCore (AC.namerText id) prog

simpAvalanche
  :: (Eq p, Show p)
  => AP.Program () Var p
  -> AP.Program () Var p
simpAvalanche av
 = let name = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) "anf"
   in  snd
     $ Fresh.runFresh go name
 where
  go = AS.simpAvalanche () av

simpFlattened
  :: AP.Program (CA.Annot ()) Var APF.Prim
  -> AP.Program () Var APF.Prim
simpFlattened av
 = let name = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) "simp"
   in  AA.eraseAnnotP
     $ snd
     $ Fresh.runFresh go' name
 where
  go'
   = go av
  -- Thread through a dummy annotation
  go
   = AS.simpFlattened (CA.Annot (CT.FunT [] CT.ErrorT) ())


coreSimp :: Program' -> Program'
coreSimp p
 = Core.condenseProgram ()
 $ snd
 $ Fresh.runFresh (Core.simpProgram () p) (freshNamer "simp")


freshNamer :: Text -> Fresh.NameState SP.Variable
freshNamer prefix = Fresh.counterPrefixNameState (SP.Variable . T.pack . show) (SP.Variable prefix)
