{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ConstraintKinds   #-}

module Icicle.Compiler
  ( ErrorCompile (..)

  , AnnotType
  , Source.QueryUntyped
  , Source.QueryTyped

  , Source.CoreProgramUntyped
  , CoreProgramTyped
  , AvalProgramUntyped
  , AvalProgramTyped

    -- * From dictionaires and libraries
  , avalancheOfDictionary
  , coreOfDictionary

    -- * Works on Source programs
  , sourceConvert

    -- * Works on Core programs
  , coreSimp
  , coreFlatten
  , coreFlatten_
  , coreAvalanche
  , fuseCore
  , coreOfSource
  , coreOfSource1

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

import qualified Icicle.Avalanche.Annot                   as Avalanche
import qualified Icicle.Avalanche.Check                   as Avalanche
import qualified Icicle.Avalanche.FromCore                as Avalanche
import qualified Icicle.Avalanche.Program                 as Avalanche
import qualified Icicle.Avalanche.Simp                    as Avalanche
import qualified Icicle.Avalanche.Statement.Flatten       as Avalanche
import qualified Icicle.Avalanche.Prim.Flat               as Flat

import qualified Icicle.Common.Annot                      as Common
import qualified Icicle.Common.Base                       as Common
import qualified Icicle.Common.Eval                       as Common
import qualified Icicle.Common.Fresh                      as Fresh
import qualified Icicle.Common.Type                       as Common

import qualified Icicle.Core.Exp.Prim                     as Core
import qualified Icicle.Core.Program.Condense             as Core
import qualified Icicle.Core.Program.Fusion               as Core
import qualified Icicle.Core.Program.Program              as Core
import qualified Icicle.Core.Program.Simp                 as Core

import           Icicle.Data

import           Icicle.Dictionary                        (Dictionary)
import qualified Icicle.Dictionary                        as Dict

import           Icicle.Internal.Pretty
import           Icicle.Internal.Rename

import qualified Icicle.Source.Parser                     as Source
import qualified Icicle.Source.Query                      as Query
import qualified Icicle.Source.ToCore.Base                as ToCore
import qualified Icicle.Source.ToCore.ToCore              as ToCore

import           Icicle.Sea.Eval                          (SeaError, seaEvalAvalanche)

import qualified Icicle.Simulator                         as Sim

import qualified Icicle.Compiler.Source                   as Source


import           Data.List.NonEmpty ( NonEmpty(..) )
import           Data.Map                                 (Map)
import qualified Data.Map                                 as M
import           Data.Monoid
import           Data.String
import           Data.Hashable                            (Hashable)

import           Control.Parallel.Strategies              (withStrategy, parTraversable, rparWith, rseq)

import qualified Text.ParserCombinators.Parsec            as Parsec

import           GHC.Generics                             (Generic)

import           P

import           System.IO                                (IO)

import           X.Control.Monad.Trans.Either


--------------------------------------------------------------------------------

type IsName v = (Hashable v, Eq v, IsString v, Pretty v, Show v, NFData v)

type AnnotType = Common.Annot Source.AnnotUnit

type CoreProgramTyped   v   = Core.Program        AnnotType v
type AvalProgramTyped   v p = Avalanche.Program   AnnotType v p
type AvalProgramUntyped v p = Avalanche.Program   Source.AnnotUnit v p

type Error = ErrorCompile Source.Var

annotUnit :: ()
annotUnit = ()

annotTypeDummy :: Common.Annot ()
annotTypeDummy = Common.Annot (Common.FunT [] Common.ErrorT) ()

--------------------------------------------------------------------------------

data ErrorCompile var
 = ErrorSource      !(Source.ErrorSource                       var                  )
 -- Core
 | ErrorConvert     !(ToCore.ConvertError     Parsec.SourcePos var                  )
 | ErrorFusion      !(Core.FusionError                         Source.Var           )
 -- Avalanche/Flatten
 | ErrorFlatten     !(Avalanche.FlattenError  Source.AnnotUnit var                  )
 | ErrorFlattenSimp !(Avalanche.SimpError     Source.AnnotUnit var         Flat.Prim)
 | ErrorAvalanche   !(Avalanche.ProgramError  Source.AnnotUnit var         Flat.Prim)
 deriving (Show, Generic)

-- deepseq stops here, we don't really care about sequencing the error
-- just need this to make sure the return type (with an AST) is sequenced.
instance NFData (ErrorCompile a) where rnf _ = ()


annotOfError :: ErrorCompile a -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ErrorSource e'
     -> Source.annotOfError e'
    ErrorConvert e'
     -> ToCore.annotOfError e'
    ErrorFusion _
     -> Nothing
    ErrorFlatten _
     -> Nothing
    ErrorFlattenSimp _
     -> Nothing
    ErrorAvalanche _
     -> Nothing

instance (Hashable a, Eq a, IsString a, Pretty a, Show a) => Pretty (ErrorCompile a) where
 pretty e
  = case e of
     ErrorSource (Source.ErrorSourceParse p)
      -> "Parse error:" <> line
      <> indent 2 (text $ show p)
     ErrorSource (Source.ErrorSourceDesugar d)
      -> "Desugar error:" <> line
      <> indent 2 (pretty d)
     ErrorSource (Source.ErrorSourceCheck ce)
      -> "Check error:" <> line
      <> indent 2 (pretty ce)
     ErrorConvert ce
      -> "Convert error:" <> line
      <> indent 2 (pretty ce)
     ErrorFusion ce
      -> "Fusion error:" <> line
      <> indent 2 (pretty ce)
     ErrorFlatten d
      -> "Flatten error:" <> line
      <> indent 2 (text $ show d)
     ErrorFlattenSimp d
      -> "Flatten simplify error:" <> line
      <> indent 2 (text $ show d)
     ErrorAvalanche d
      -> "Program error:" <> line
      <> indent 2 (text $ show d)

--------------------------------------------------------------------------------

-- * Compile

sourceConvert :: Dictionary
              -> Source.QueryTyped Source.Var
              -> Either Error (Source.CoreProgramUntyped Source.Var)
sourceConvert d q
 = second snd
 $ first ErrorConvert conv
 where
  convDict = Dict.featureMapOfDictionary d
  conv     = Fresh.runFreshT
              (ToCore.convertQueryTop convDict q)
              (freshNamer "conv")

----------------------------------------
-- * core

coreOfDictionary :: Source.IcicleCompileOptions
                 -> Dictionary
                 -> Either Error (Map Attribute (NonEmpty (Source.CoreProgramUntyped Source.Var)))
coreOfDictionary opts dict = do
  let virtuals   = fmap (second Dict.unVirtual) (Dict.virtualFeaturesIn dict)
  let fusionOpts = Source.icicleFusionOptions opts

  core  <- parTraverse (coreOfSource opts dict) virtuals
  fused <- parTraverse (fuseCore fusionOpts)   (M.unionsWith (<>) core)

  return fused

coreSimp :: IsName v
         => Source.CoreProgramUntyped v
         -> Source.CoreProgramUntyped v
coreSimp p
 = Core.condenseProgram ()
 $!! snd
 $!! Fresh.runFresh (Core.simpProgram annotUnit p) (freshNamer "simp")


fuseCore :: Source.FusionOptions
         -> [(Source.Var, Source.CoreProgramUntyped Source.Var)]
         -> Either Error (NonEmpty (Source.CoreProgramUntyped Source.Var))
fuseCore opts programs = do
  fs <- mapM go $ chunk programs
  case fs of
   [] -> Left $ ErrorFusion Core.FusionErrorNothingToFuse
   (p:ps) -> return (p :| ps)
 where
  go ps
   = first ErrorFusion $ do
      fused <- Core.fuseMultiple annotUnit ps
      pure (coreSimp fused)

  chunk [] = []
  chunk ps
   = let (as,bs) = splitAt (Source.fusionMaximumPerKernel opts) ps
     in  as : chunk bs


coreOfSource :: Source.IcicleCompileOptions
             -> Dictionary
             -> (Attribute, Source.QueryTyped Source.Var)
             -> Either Error (Map Attribute [(Source.Var, Source.CoreProgramUntyped Source.Var)])
coreOfSource opt dict (Attribute attr, virtual) = do
  core <- coreOfSource1 opt dict virtual
  let baseattr    = (Attribute . unVar . unName) (Query.feature virtual)
  pure (M.singleton baseattr [(Source.Variable attr, core)])

coreOfSource1 :: Source.IcicleCompileOptions
             -> Dictionary
             -> Source.QueryTyped Source.Var
             -> Either Error (Source.CoreProgramUntyped Source.Var)
coreOfSource1 opt dict virtual = do
  let inlined     = Source.sourceInline    (Source.icicleInline  opt) dict virtual
  desugared      <- first ErrorSource $ Source.sourceDesugarQT  inlined
  (checked, _)   <- first ErrorSource $ Source.sourceCheckQT   (Source.icicleBigData opt) dict desugared
  let reified     = Source.sourceReifyQT checked
  core           <- sourceConvert dict reified
  return $ coreSimp core


----------------------------------------
-- * avalanche

avalancheOfDictionary :: Source.IcicleCompileOptions
                      -> Dictionary
                      -> Either Error (Map Attribute (NonEmpty (AvalProgramTyped Source.Var Flat.Prim)))
avalancheOfDictionary opts dict = do
  let virtuals   = fmap (second Dict.unVirtual) (Dict.virtualFeaturesIn dict)
  let fusionOpts = Source.icicleFusionOptions opts

  core      <- parTraverse (coreOfSource opts dict)   virtuals
  fused     <- parTraverse (fuseCore fusionOpts)     (M.unionsWith (<>) core)
  avalanche <- parTraverse (traverse avalancheOfCore) fused

  return avalanche


avalancheOfCore ::               Source.CoreProgramUntyped Source.Var
                -> Either Error (AvalProgramTyped   Source.Var Flat.Prim)
avalancheOfCore core = do
  flat    <- coreFlatten core
  checked <- checkAvalanche flat
  return checked


coreFlatten :: IsName v
            => Source.CoreProgramUntyped v
            -> Either (ErrorCompile v) (AvalProgramUntyped v Flat.Prim)
coreFlatten = coreFlatten_ Avalanche.defaultSimpOpts


coreFlatten_ :: IsName v
             => Avalanche.SimpOpts
             -> Source.CoreProgramUntyped v
             -> Either (ErrorCompile v) (AvalProgramUntyped v Flat.Prim)
coreFlatten_ opts prog
 =   join
 $!! fmap (simpFlattened opts)
 $!! flattenAvalanche
 $!! coreAvalanche prog


flattenAvalanche :: IsName v
                 => AvalProgramUntyped v Core.Prim
                 -> Either (ErrorCompile v) (AvalProgramTyped   v Flat.Prim)
flattenAvalanche av
 = join
 . second snd
 . first ErrorFlatten
 $!! Fresh.runFreshT go (freshNamer "flat")
 where
  go = do s' <- Avalanche.flatten annotUnit (Avalanche.statements av)
          return $ checkAvalanche (av { Avalanche.statements = force s' })


checkAvalanche :: IsName v
               => AvalProgramUntyped v Flat.Prim
               -> Either (ErrorCompile v) (AvalProgramTyped   v Flat.Prim)
checkAvalanche prog
 = first ErrorAvalanche
 $ Avalanche.checkProgram Flat.flatFragment prog


coreAvalanche :: IsName v
              => Source.CoreProgramUntyped v
              -> AvalProgramUntyped v Core.Prim
coreAvalanche prog
 = simpAvalanche
 $ snd
 $ Fresh.runFresh (Avalanche.programFromCore (Avalanche.namerText id) prog) (freshNamer "aval")


simpAvalanche :: IsName v
              => AvalProgramUntyped v Core.Prim
              -> AvalProgramUntyped v Core.Prim
simpAvalanche av
 = snd
 $ Fresh.runFresh go (freshNamer "anf")
 where
  go = Avalanche.simpAvalanche annotUnit av


simpFlattened :: IsName v
              => Avalanche.SimpOpts
              -> AvalProgramTyped   v Flat.Prim
              -> Either (ErrorCompile v) (AvalProgramUntyped v Flat.Prim)
simpFlattened opts av
 = first ErrorFlattenSimp . second Avalanche.eraseAnnotP . snd
 $ Fresh.runFresh (go av) (freshNamer "simpflat")
 where
  -- Thread through a dummy annotation
  go = Avalanche.simpFlattened annotTypeDummy opts


--------------------------------------------------------------------------------

-- * Eval

type SimError = Sim.SimulateError Source.AnnotUnit Source.Var

newtype Result   = Result (Entity, Value)
  deriving (Eq, Show)

instance Pretty Result where
  pretty (Result (ent, val))
    = pretty ent <> comma <> space <> pretty val

coreEval :: Common.EvalContext
         -> [AsAt Fact]
         -> Source.QueryTyped Source.Var
         -> Source.CoreProgramUntyped  Source.Var
         -> Either SimError [Result]
coreEval ctx fs (renameQT unVar -> query) prog
 = do let partitions = Sim.streams fs
      let feat       = Query.feature query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (Sim.Partition ent attr values)
      | Common.NameBase feat' <- Common.nameBase feat
      , attr == Attribute feat'
      = do  (vs',_) <- evalV values
            return $ fmap (\v -> Result (ent, snd v)) vs'

      | otherwise
      = return []

    evalV
      = Sim.evaluateVirtualValue prog ctx

avalancheEval :: Common.EvalContext
              -> [AsAt Fact]
              -> Source.QueryTyped Source.Var
              -> AvalProgramUntyped Source.Var Flat.Prim
              -> Either SimError [Result]
avalancheEval ctx fs (renameQT unVar -> query) prog
 = do let partitions = Sim.streams fs
      let feat       = Query.feature query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (Sim.Partition ent attr values)
      | Common.NameBase feat' <- Common.nameBase feat
      , attr == Attribute feat'
      = do  (vs',_) <- evalV values
            return $ fmap (\v -> Result (ent, snd v)) vs'

      | otherwise
      = return []

    evalV
      = Sim.evaluateVirtualValue' prog ctx

seaEval :: Common.EvalContext
        -> [AsAt Fact]
        -> Source.QueryTyped Source.Var
        -> AvalProgramTyped  Source.Var Flat.Prim
        -> EitherT SeaError IO [Result]
seaEval ctx newFacts (renameQT unVar -> query) program =
  fmap Result . mconcat <$> sequence results
  where
    partitions :: [Sim.Partition]
    partitions  = Sim.streams newFacts

    results :: [EitherT SeaError IO [(Entity, Value)]]
    results = fmap (evalP (Query.feature query)) partitions

    evalP :: Common.Name Text
          -> Sim.Partition
          -> EitherT SeaError IO [(Entity, Value)]
    evalP featureName (Sim.Partition entityName attributeName values)
      | Common.NameBase name <- Common.nameBase featureName
      , Attribute name == attributeName
      = do outputs <- seaEvalAvalanche program ctx values
           return $ fmap (\out -> (entityName, snd out)) outputs

      | otherwise
      = return []

--------------------------------------------------------------------------------

unVar :: Source.Variable -> Text
unVar (Source.Variable x) = x

unName :: Common.Name a -> a
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
