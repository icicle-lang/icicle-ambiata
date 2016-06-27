{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ConstraintKinds   #-}

module Icicle.Pipeline
  ( CompileError(..)

    -- * Type aliases for different types of programs
  , IsName

  , SourceVar
  , SourceAnnot

  , AnnotUnit
  , AnnotType

  , QueryTopPosUntyped
  , QueryTopPosTyped

  , CoreProgramUntyped
  , CoreProgramTyped
  , AvalProgramUntyped
  , AvalProgramTyped

  , Funs
  , FunEnvT

    -- * Compiler options
  , IcicleCompileOptions (..)
  , defaultCompileOptions
  , STI.InlineOption (..)
  , STI.defaultInline
  , SC.CheckOptions (..)
  , SC.defaultCheckOptions

    -- * From dictionaires and libraries
  , queryOfSource
  , entryOfQuery
  , readIcicleLibrary
  , avalancheOfDictionary
  , coreOfDictionary

    -- * Works on Source programs
  , sourceParseQT
  , sourceParseF
  , sourceDesugarQT
  , sourceDesugarF
  , sourceReifyQT
  , sourceCheckQT
  , sourceCheckF
  , sourceConvert
  , sourceInline

    -- * Works on Core programs
  , coreSimp
  , coreFlatten
  , coreFlatten_
  , coreAvalanche
  , fuseCore
  , coreOfSource

    -- * Works on Avalanche programs
  , flattenAvalanche
  , checkAvalanche
  , simpAvalanche
  , simpFlattened
  , avalancheOfCore

    -- * Eval
  , coreEval
  , avalancheEval
  , seaEval

    -- * Helpers
  , freshNamer
  , annotOfError
  , unName
  , unVar
  , parTraverse
  ) where

import qualified Icicle.Avalanche.Annot                   as AA
import qualified Icicle.Avalanche.Check                   as AC
import qualified Icicle.Avalanche.FromCore                as AC
import qualified Icicle.Avalanche.Prim.Flat               as APF
import qualified Icicle.Avalanche.Program                 as AP
import qualified Icicle.Avalanche.Simp                    as AS
import qualified Icicle.Avalanche.Statement.Flatten       as AS

import qualified Icicle.Common.Annot                      as Common
import           Icicle.Common.Base                       (Name)
import qualified Icicle.Common.Base                       as Common
import qualified Icicle.Common.Fresh                      as Fresh
import qualified Icicle.Common.Type                       as Common

import qualified Icicle.Core.Exp.Prim                     as Core
import qualified Icicle.Core.Program.Condense             as Core
import qualified Icicle.Core.Program.Fusion               as Core
import qualified Icicle.Core.Program.Program              as Core
import qualified Icicle.Core.Program.Simp                 as Core

import           Icicle.Dictionary                        (Dictionary)
import qualified Icicle.Dictionary                        as Dict

import           Icicle.Internal.Pretty
import           Icicle.Internal.Rename

import qualified Icicle.Source.Checker                    as SC
import qualified Icicle.Source.Parser                     as SP
import qualified Icicle.Source.Query                      as SQ
import qualified Icicle.Source.ToCore.Base                as STC
import qualified Icicle.Source.ToCore.ToCore              as STC
import qualified Icicle.Source.Transform.Desugar          as STD
import qualified Icicle.Source.Transform.Inline           as STI
import qualified Icicle.Source.Transform.ReifyPossibility as STR
import qualified Icicle.Source.Type                       as ST

import           Icicle.Data

import           Icicle.Sea.Eval                          (SeaError, seaEvalAvalanche)

import qualified Icicle.Simulator                         as S


import           Data.Functor.Identity
import           Data.Map                                 (Map)
import qualified Data.Map                                 as M
import           Data.Monoid
import           Data.String
import           Data.Hashable                            (Hashable)

import           Control.Parallel.Strategies              (withStrategy, parTraversable, rparWith, rseq)

import           Text.ParserCombinators.Parsec            (SourcePos, SourceName)
import qualified Text.ParserCombinators.Parsec            as Parsec

import           GHC.Generics                             (Generic)

import           P

import           System.IO                                (IO)

import           X.Control.Monad.Trans.Either


--------------------------------------------------------------------------------

data IcicleCompileOptions = IcicleCompileOptions
  { icicleInline  :: STI.InlineOption
  , icicleBigData :: SC.CheckOptions
  }

defaultCompileOptions :: IcicleCompileOptions
defaultCompileOptions = IcicleCompileOptions STI.defaultInline SC.defaultCheckOptions

data CompileError var
 = CompileErrorParse       !Parsec.ParseError
 -- Source
 | CompileErrorDesugar     !(STD.DesugarError SourcePos var)
 | CompileErrorCheck       !(SC.CheckError    SourcePos var)
 -- Core
 | CompileErrorConvert     !(STC.ConvertError SourcePos var)
 | CompileErrorFusion      !(Core.FusionError SourceVar)
 -- Avalanche/Flatten
 | CompileErrorFlatten     !(AS.FlattenError  AnnotUnit var)
 | CompileErrorFlattenSimp !(AS.SimpError     AnnotUnit var APF.Prim)
 | CompileErrorAvalanche   !(AC.ProgramError  AnnotUnit var APF.Prim)
 deriving (Show, Generic)

-- deepseq stops here, we don't really care about sequencing the error
-- just need this to make sure the return type (with an AST) is sequenced.
instance NFData (CompileError a) where rnf _ = ()


annotOfError :: CompileError a -> Maybe SourcePos
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
    CompileErrorFusion _
     -> Nothing
    CompileErrorFlatten _
     -> Nothing
    CompileErrorFlattenSimp _
     -> Nothing
    CompileErrorAvalanche _
     -> Nothing

instance (Hashable a, Eq a, IsString a, Pretty a, Show a) => Pretty (CompileError a) where
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
     CompileErrorFusion ce
      -> "Fusion error:" <> line
      <> indent 2 (pretty ce)
     CompileErrorFlatten d
      -> "Flatten error:" <> line
      <> indent 2 (text $ show d)
     CompileErrorFlattenSimp d
      -> "Flatten simplify error:" <> line
      <> indent 2 (text $ show d)
     CompileErrorAvalanche d
      -> "Program error:" <> line
      <> indent 2 (text $ show d)

--------------------------------------------------------------------------------

-- * Compile


type IsName v = (Hashable v, Eq v, IsString v, Pretty v, Show v, NFData v)

type SourceVar     = SP.Variable
type SourceAnnot a = ST.Annot a SourceVar

type AnnotUnit = ()
type AnnotType = Common.Annot AnnotUnit

type QueryTopPosUntyped v = SQ.QueryTop              SourcePos  v
type QueryTopPosTyped   v = SQ.QueryTop (SourceAnnot SourcePos) v

type CoreProgramTyped v   = Core.Program AnnotType v
type AvalProgramTyped v p = AP.Program   AnnotType v p

type CoreProgramUntyped v   = Core.Program AnnotUnit v
type AvalProgramUntyped v p = AP.Program   AnnotUnit v p

type Funs a b = [((a, Name b), SQ.Function a b)]
type FunEnvT a b = [ ( Name b
                   , ( ST.FunctionType b
                     , SQ.Function (SourceAnnot a) b )) ]

type Error = CompileError SourceVar

annotUnit :: ()
annotUnit = ()

annotTypeDummy :: Common.Annot ()
annotTypeDummy = Common.Annot (Common.FunT [] Common.ErrorT) ()

----------------------------------------
-- * queries

queryOfSource :: SC.CheckOptions
              -> Dictionary
              -> Text
              -> Text
              -> Text
              -> Either Error (Attribute, QueryTopPosTyped SourceVar)
queryOfSource checkOpts dict name src namespace = do
  parsed       <- sourceParseQT name (Namespace namespace) src
  desugared    <- sourceDesugarQT parsed
  (checked, _) <- sourceCheckQT checkOpts dict desugared
  pure (Attribute name, checked)

entryOfQuery :: Attribute -> (QueryTopPosTyped SourceVar) -> Text -> Dict.DictionaryEntry
entryOfQuery attr query nsp
  = Dict.DictionaryEntry attr (Dict.VirtualDefinition (Dict.Virtual query)) (Namespace nsp)

----------------------------------------
-- * source

sourceParseQT :: Text
              -> Namespace
              -> Text
              -> Either Error (QueryTopPosUntyped SourceVar)
sourceParseQT base namespace t
 = first CompileErrorParse
 $ SP.parseQueryTop (Common.OutputName base namespace) t

sourceParseF :: SourceName
             -> Text
             -> Either Error (Funs SourcePos SourceVar)
sourceParseF env t
 = first CompileErrorParse
 $ SP.parseFunctions env t

sourceDesugarQT ::               QueryTopPosUntyped SourceVar
                -> Either Error (QueryTopPosUntyped SourceVar)
sourceDesugarQT q
 = runIdentity . runEitherT . bimapEitherT CompileErrorDesugar snd
 $ Fresh.runFreshT
     (STD.desugarQT q)
     (freshNamer "desugar_q")

sourceDesugarF :: Funs SourcePos SourceVar
               -> Either (CompileError SourceVar) (Funs SourcePos SourceVar)
sourceDesugarF fun
 = runIdentity . runEitherT . bimapEitherT CompileErrorDesugar snd
 $ Fresh.runFreshT
     (mapM (mapM STD.desugarFun) fun)
     (freshNamer "desugar_f")

sourceReifyQT :: QueryTopPosTyped SourceVar
              -> QueryTopPosTyped SourceVar
sourceReifyQT q
 = snd
 $ runIdentity
 $ Fresh.runFreshT
     (STR.reifyPossibilityQT q)
     (freshNamer "reify")

sourceCheckQT :: SC.CheckOptions
              -> Dictionary
              -> QueryTopPosUntyped SourceVar
              -> Either Error (QueryTopPosTyped SourceVar, ST.Type SourceVar)
sourceCheckQT opts d q
 = let d' = Dict.featureMapOfDictionary d
   in  first CompileErrorCheck
     $ snd
     $ flip Fresh.runFresh (freshNamer "check")
     $ runEitherT
     $ SC.checkQT opts d' q

sourceCheckF :: FunEnvT SourcePos SourceVar
             -> Funs    SourcePos SourceVar
             -> Either  Error (FunEnvT SourcePos SourceVar)
sourceCheckF env parsedImport
 = first CompileErrorCheck
 $ snd
 $ flip Fresh.runFresh (freshNamer "check")
 $ runEitherT
 $ SC.checkFs env parsedImport

sourceInline :: STI.InlineOption
             -> Dictionary
             -> QueryTopPosTyped   SourceVar
             -> QueryTopPosUntyped SourceVar
sourceInline opt d q
 = SQ.reannotQT ST.annAnnot
 $ inline q
 where
  funs      = M.map snd
            $ M.fromList
            $ Dict.dictionaryFunctions d
  inline q' = snd
            $ Fresh.runFresh
                (STI.inlineQT opt funs q')
                (freshNamer "inline")

sourceConvert :: Dictionary
              -> QueryTopPosTyped SourceVar
              -> Either Error (CoreProgramUntyped SourceVar)
sourceConvert d q
 = second snd
 $ first CompileErrorConvert conv
 where
  d'        = Dict.featureMapOfDictionary d
  conv      = Fresh.runFreshT
                (STC.convertQueryTop d' q)
                (freshNamer "conv")

----------------------------------------
-- * core

coreOfDictionary :: IcicleCompileOptions
                 -> Dictionary
                 -> Either Error (Map Attribute (CoreProgramUntyped SourceVar))
coreOfDictionary opts dict = do
  let virtuals = fmap (second Dict.unVirtual) (Dict.getVirtualFeatures dict)

  core  <- parTraverse (coreOfSource opts dict) virtuals
  fused <- parTraverse  fuseCore               (M.unionsWith (<>) core)

  return fused

coreSimp :: IsName v
         => CoreProgramUntyped v
         -> CoreProgramUntyped v
coreSimp p
 = Core.condenseProgram ()
 $!! snd
 $!! Fresh.runFresh (Core.simpProgram annotUnit p) (freshNamer "simp")


fuseCore :: [(SourceVar, CoreProgramUntyped SourceVar)]
         -> Either Error (CoreProgramUntyped SourceVar)
fuseCore programs = first CompileErrorFusion $ do
  fused <- Core.fuseMultiple annotUnit programs
  pure (coreSimp fused)


coreOfSource :: IcicleCompileOptions
             -> Dictionary
             -> (Attribute, QueryTopPosTyped SourceVar)
             -> Either Error (Map Attribute [(SourceVar, CoreProgramUntyped SourceVar)])
coreOfSource opt dict (Attribute attr, virtual) = do
  let inlined     = sourceInline    (icicleInline  opt) dict virtual
  desugared      <- sourceDesugarQT  inlined
  (checked, _)   <- sourceCheckQT   (icicleBigData opt) dict desugared
  let reified     = sourceReifyQT checked
  core           <- sourceConvert dict reified
  let simplified  = coreSimp core
  let baseattr    = (Attribute . unVar . unName) (SQ.feature virtual)

  pure (M.singleton baseattr [(SP.Variable attr, simplified)])


----------------------------------------
-- * avalanche

avalancheOfDictionary :: IcicleCompileOptions
                      -> Dictionary
                      -> Either Error (Map Attribute (AvalProgramTyped SourceVar APF.Prim))
avalancheOfDictionary opt dict = do
  let virtuals = fmap (second Dict.unVirtual) (Dict.getVirtualFeatures dict)

  core      <- parTraverse (coreOfSource opt dict) virtuals
  fused     <- parTraverse fuseCore                (M.unionsWith (<>) core)
  avalanche <- parTraverse avalancheOfCore         fused

  return avalanche


avalancheOfCore ::               CoreProgramUntyped SourceVar
                -> Either Error (AvalProgramTyped   SourceVar APF.Prim)
avalancheOfCore core = do
  flat    <- coreFlatten core
  checked <- checkAvalanche flat
  return checked


coreFlatten :: IsName v
            =>                          CoreProgramUntyped v
            -> Either (CompileError v) (AvalProgramUntyped v APF.Prim)
coreFlatten = coreFlatten_ AS.defaultSimpOpts


coreFlatten_ :: IsName v
             => AS.SimpOpts
             ->                          CoreProgramUntyped v
             -> Either (CompileError v) (AvalProgramUntyped v APF.Prim)
coreFlatten_ opts prog
 =   join
 $!! fmap (simpFlattened opts)
 $!! flattenAvalanche
 $!! coreAvalanche prog


flattenAvalanche :: IsName v
                 =>                          AvalProgramUntyped v Core.Prim
                 -> Either (CompileError v) (AvalProgramTyped   v APF.Prim)
flattenAvalanche av
 = join
 . second snd
 . first CompileErrorFlatten
 $!! Fresh.runFreshT go (freshNamer "flat")
 where
  go = do s' <- AS.flatten annotUnit (AP.statements av)
          return $ checkAvalanche (av { AP.statements = force s' })


checkAvalanche :: IsName v
               =>                          AvalProgramUntyped v APF.Prim
               -> Either (CompileError v) (AvalProgramTyped   v APF.Prim)
checkAvalanche prog
 = first CompileErrorAvalanche
 $ AC.checkProgram APF.flatFragment prog


coreAvalanche :: IsName v
              => CoreProgramUntyped v
              -> AvalProgramUntyped v Core.Prim
coreAvalanche prog
 = simpAvalanche
 $ snd
 $ Fresh.runFresh (AC.programFromCore (AC.namerText id) prog) (freshNamer "aval")


simpAvalanche :: IsName v
              => AvalProgramUntyped v Core.Prim
              -> AvalProgramUntyped v Core.Prim
simpAvalanche av
 = snd
 $ Fresh.runFresh go (freshNamer "anf")
 where
  go = AS.simpAvalanche annotUnit av


simpFlattened :: IsName v
              => AS.SimpOpts
              ->                          AvalProgramTyped   v APF.Prim
              -> Either (CompileError v) (AvalProgramUntyped v APF.Prim)
simpFlattened opts av
 = first CompileErrorFlattenSimp . second AA.eraseAnnotP . snd
 $ Fresh.runFresh (go av) (freshNamer "simpflat")
 where
  -- Thread through a dummy annotation
  go = AS.simpFlattened annotTypeDummy opts


----------------------------------------
-- * library

readIcicleLibrary :: SourceVar -> SourceName -> Text -> Either Error (FunEnvT SourcePos SourceVar)
readIcicleLibrary name source input
 = do input' <- first CompileErrorParse $ SP.parseFunctions source input
      first CompileErrorCheck
             $ snd
             $ flip Fresh.runFresh (freshNamer name)
             $ runEitherT
             $ SC.checkFs [] input'

--------------------------------------------------------------------------------

-- * Eval

type SimError = S.SimulateError AnnotUnit SourceVar

newtype Result   = Result (Entity, Value)
  deriving (Eq, Show)

instance Pretty Result where
  pretty (Result (ent, val))
    = pretty ent <> comma <> space <> pretty val

coreEval :: Time
         -> [AsAt Fact]
         -> QueryTopPosTyped SourceVar
         -> CoreProgramUntyped  SourceVar
         -> Either SimError [Result]
coreEval t fs (renameQT unVar -> query) prog
 = do let partitions = S.streams fs
      let feat       = SQ.feature query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (S.Partition ent attr values)
      | Common.NameBase feat' <- Common.nameBase feat
      , attr == Attribute feat'
      = do  (vs',_) <- evalV values
            return $ fmap (\v -> Result (ent, snd v)) vs'

      | otherwise
      = return []

    evalV
      = S.evaluateVirtualValue prog t

avalancheEval :: Time
              -> [AsAt Fact]
              -> QueryTopPosTyped SourceVar
              -> AvalProgramUntyped SourceVar APF.Prim
              -> Either SimError [Result]
avalancheEval t fs (renameQT unVar -> query) prog
 = do let partitions = S.streams fs
      let feat       = SQ.feature query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (S.Partition ent attr values)
      | Common.NameBase feat' <- Common.nameBase feat
      , attr == Attribute feat'
      = do  (vs',_) <- evalV values
            return $ fmap (\v -> Result (ent, snd v)) vs'

      | otherwise
      = return []

    evalV
      = S.evaluateVirtualValue' prog t

seaEval :: Time
        -> [AsAt Fact]
        -> QueryTopPosTyped  SourceVar
        -> AvalProgramTyped SourceVar APF.Prim
        -> EitherT SeaError IO [Result]
seaEval t newFacts (renameQT unVar -> query) program =
  fmap Result . mconcat <$> sequence results
  where
    partitions :: [S.Partition]
    partitions  = S.streams newFacts

    results :: [EitherT SeaError IO [(Entity, Value)]]
    results = fmap (evalP (SQ.feature query)) partitions

    evalP :: Common.Name Text
          -> S.Partition
          -> EitherT SeaError IO [(Entity, Value)]
    evalP featureName (S.Partition entityName attributeName values)
      | Common.NameBase name <- Common.nameBase featureName
      , Attribute name == attributeName
      = do outputs <- seaEvalAvalanche program t values
           return $ fmap (\out -> (entityName, snd out)) outputs

      | otherwise
      = return []

--------------------------------------------------------------------------------

unVar :: SP.Variable -> Text
unVar (SP.Variable x) = x

unName :: Name a -> a
unName = go . Common.nameBase
  where
   go (Common.NameBase  x) = x
   go (Common.NameMod _ x) = go x

freshNamer :: IsString v => v -> Fresh.NameState v
freshNamer prefix
 = Fresh.counterPrefixNameState (fromString . show) prefix

parTraverse  :: Traversable t => (a -> Either e b) -> t a -> Either e (t b)
parTraverse f = sequenceA . parallel . fmap f
 where
  parallel = withStrategy (parTraversable (rparWith rseq))
