{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Icicle.Compiler (
    ErrorCompile(..)

  , AnnotType
  , Source.QueryUntyped
  , Source.QueryTyped

  , Source.CoreProgramUntyped
  , CoreProgramTyped
  , AvalProgramUntyped
  , AvalProgramTyped

  , Result(..)

  , CompilationStatus(..)
  , CompilationPhase(..)

    -- * From dictionaires and libraries
  , avalancheOfDictionary
  , avalancheOfDictionaryM
  , coreOfDictionary

    -- * Works on Source programs
  , sourceConvert
  , sourceInputId

    -- * Works on Core programs
  , coreSimp
  , coreFlatten
  , coreFlatten_
  , coreAvalanche
  , fuseCore
  , coreOfSource
  , coreOfSource1
  , checkCore

    -- * Works on Avalanche programs
  , flattenAvalanche
  , checkAvalanche
  , simpAvalanche
  , simpFlattened
  , avalancheOfCore

    -- * Eval
  , coreEval
  , avalancheEval

    -- * Helpers
  , freshNamer
  , annotOfError
  , unName
  , unVar
  , parTraverse
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Parallel.Strategies (withStrategy, parTraversable, rparWith, rdeepseq)

import           Data.Functor.Identity (runIdentity)
import           Data.Hashable (Hashable)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (IsString(..))
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import qualified Icicle.Avalanche.Annot as Avalanche
import qualified Icicle.Avalanche.Check as Avalanche
import qualified Icicle.Avalanche.FromCore as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import qualified Icicle.Avalanche.Simp as Avalanche
import qualified Icicle.Avalanche.Statement.Flatten as Avalanche
import qualified Icicle.Avalanche.Prim.Flat as Flat

import qualified Icicle.Common.Annot as Common
import qualified Icicle.Common.Base as Common
import qualified Icicle.Common.Eval as Common
import qualified Icicle.Common.Fresh as Fresh
import qualified Icicle.Common.Type as Common

import qualified Icicle.Core.Exp.Prim as Core
import qualified Icicle.Core.Program.Check as Core
import qualified Icicle.Core.Program.Condense as Core
import qualified Icicle.Core.Program.Error as Core
import qualified Icicle.Core.Program.Fusion as Core
import qualified Icicle.Core.Program.Program as Core
import qualified Icicle.Core.Program.Simp as Core

import           Icicle.Data

import           Icicle.Dictionary (Dictionary)
import qualified Icicle.Dictionary as Dict

import           Icicle.Internal.Pretty
import           Icicle.Internal.Rename

import qualified Icicle.Source.Parser as Source
import qualified Icicle.Source.Query as Query
import qualified Icicle.Source.ToCore.Base as ToCore
import qualified Icicle.Source.ToCore.ToCore as ToCore

import qualified Icicle.Simulator as Sim

import qualified Icicle.Compiler.Source as Source

import           P

import qualified Text.ParserCombinators.Parsec as Parsec

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)


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
 | ErrorCoreCheck   !(Core.ProgramError       Source.AnnotUnit var                  )
 -- Avalanche/Flatten
 | ErrorFlatten     !(Avalanche.FlattenError  Source.AnnotUnit var                  )
 | ErrorFlattenSimp !(Avalanche.SimpError     Source.AnnotUnit var         Flat.Prim)
 | ErrorAvalanche   !(Avalanche.ProgramError  Source.AnnotUnit var         Flat.Prim)
 deriving (Show, Generic)

-- FIXME We can't implement NFData properly for this type because Parsec.SourcePos is
-- FIXME not NFData, we really should define our own type for source positions.
instance NFData (ErrorCompile a) where rnf x = seq x ()


annotOfError :: ErrorCompile a -> Maybe Parsec.SourcePos
annotOfError e
 = case e of
    ErrorSource e'
     -> Source.annotOfError e'
    ErrorConvert e'
     -> ToCore.annotOfError e'
    ErrorCoreCheck _
     -> Nothing
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
     ErrorSource se
      -> "Source error:" <> line
      <> indent 2 (pretty se)
     ErrorConvert ce
      -> "Convert error:" <> line
      <> indent 2 (pretty ce)
     ErrorFusion ce
      -> "Fusion error:" <> line
      <> indent 2 (pretty ce)
     ErrorCoreCheck ce
      -> "Core error:" <> line
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
                 -> Either Error (Map InputId (NonEmpty (Source.CoreProgramUntyped Source.Var)))
coreOfDictionary opts dict = do
  let queries    = fmap Dict.outputQuery . Map.elems $ Dict.dictionaryOutputs dict
  let fusionOpts = Source.icicleFusionOptions opts

  core  <- parTraverse (coreOfSource opts dict) queries
  fused <- parTraverse (fuseCore fusionOpts)   (Map.unionsWith (<>) core)

  return fused

coreSimp :: IsName v
         => Source.CoreProgramUntyped v
         -> Source.CoreProgramUntyped v
coreSimp p
 = Core.condenseProgram ()
 $!! snd
 $!! Fresh.runFresh (Core.simpProgram annotUnit p) (freshNamer "simp")


fuseCore :: Source.FusionOptions
         -> [Source.CoreProgramUntyped Source.Var]
         -> Either Error (NonEmpty (Source.CoreProgramUntyped Source.Var))
fuseCore opts programs = do
  fs <- mapM go $ chunk programs
  case fs of
   [] -> Left $ ErrorFusion Core.FusionErrorNothingToFuse
   (p:ps) -> return (p :| ps)
 where
  go ps
   = first ErrorFusion $ do
      fused <- Core.fuseMultiple annotUnit $ indexed ps
      pure (coreSimp fused)

  indexed ps
   = List.zipWith (\ix p -> (Source.Variable . Text.pack $ show ix, p)) ([0..] :: [Int]) ps

  chunk [] = []
  chunk ps
   = let (as,bs) = splitAt (Source.fusionMaximumPerKernel opts) ps
     in  as : chunk bs

sourceInputId :: Source.QueryTyped Source.Var -> Dictionary -> Either Error InputId
sourceInputId query d =
  let
    iid =
      Query.queryInput query
  in
    maybeToRight (ErrorSource (Source.ErrorSourceResolveError iid)) $
      resolveInputId iid (Map.keys $ Dict.dictionaryInputs d)

coreOfSource :: Source.IcicleCompileOptions
             -> Dictionary
             -> Source.QueryTyped Source.Var
             -> Either Error (Map InputId [Source.CoreProgramUntyped Source.Var])
coreOfSource opt dict query = do
  core <- coreOfSource1 opt dict query
  iid <- sourceInputId query dict
  pure (Map.singleton iid [core])

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
  _              <- checkCore core
  return $ coreSimp core

checkCore      :: IsName v
               => Source.CoreProgramUntyped v
               -> Either (ErrorCompile v) [(OutputId, Common.Type)]
checkCore prog
 = first ErrorCoreCheck
 $ Core.checkProgram prog


----------------------------------------
-- * avalanche

data CompilationPhase =
    PhaseSourceToCore
  | PhaseFuseCore
  | PhaseCoreToAvalanche

data CompilationStatus =
    CompileBegin CompilationPhase
  | CompileEnd CompilationPhase

avalancheOfDictionary ::
     Source.IcicleCompileOptions
  -> Dictionary
  -> Either Error (Map InputId (NonEmpty (AvalProgramTyped Source.Var Flat.Prim)))
avalancheOfDictionary options dictionary =
  runIdentity . runEitherT $ avalancheOfDictionaryM (const $ pure ()) options dictionary

avalancheOfDictionaryM ::
     Monad m
  => (CompilationStatus -> m ())
  -> Source.IcicleCompileOptions
  -> Dictionary
  -> EitherT Error m (Map InputId (NonEmpty (AvalProgramTyped Source.Var Flat.Prim)))
avalancheOfDictionaryM updateUI opts dict = do
  let queries    = fmap Dict.outputQuery . Map.elems $ Dict.dictionaryOutputs dict
  let fusionOpts = Source.icicleFusionOptions opts

  lift . updateUI $ CompileBegin PhaseSourceToCore
  core <- hoistEither $ parTraverse (coreOfSource opts dict) queries
  lift . updateUI $ CompileEnd PhaseSourceToCore

  lift . updateUI $ CompileBegin PhaseFuseCore
  fused <- hoistEither $ parTraverse (fuseCore fusionOpts) (Map.unionsWith (<>) core)
  lift . updateUI $ CompileEnd PhaseFuseCore

  lift . updateUI $ CompileBegin PhaseCoreToAvalanche
  avalanche <- hoistEither $ parTraverse (parTraverse avalancheOfCore) fused
  lift . updateUI $ CompileEnd PhaseCoreToAvalanche

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
  go = do s'  <- Avalanche.flatten annotUnit (Avalanche.statements av)
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
      let feat       = Query.queryInput query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (Sim.Partition ent attr values)
      | attr == unresolvedInputName feat
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
      let feat       = Query.queryInput query
      let results    = fmap (evalP feat) partitions

      res' <- sequence results

      return $ concat res'

  where
    evalP feat (Sim.Partition ent attr values)
      | attr == unresolvedInputName feat
      = do  (vs',_) <- evalV values
            return $ fmap (\v -> Result (ent, snd v)) vs'

      | otherwise
      = return []

    evalV
      = Sim.evaluateVirtualValue' prog ctx

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

parTraverse  :: (NFData x, NFData b, Traversable t) => (a -> Either x b) -> t a -> Either x (t b)
parTraverse f xs =
  let
    parallel =
      withStrategy (parTraversable (rparWith rdeepseq))
  in
    sequenceA (parallel (fmap f xs))
