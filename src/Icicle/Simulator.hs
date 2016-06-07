-- | This needs cleaning up but the idea is there
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Simulator (
    streams
  , Partition(..)
  , SimulateError
  , evaluateVirtualValue
  , evaluateVirtualValue'
  ) where

import           Data.List
import           Data.Hashable (Hashable)

import           P

import qualified Icicle.BubbleGum   as B
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Data (valueToCore, valueFromCore)
import           Icicle.Data

import           Icicle.Internal.Pretty

import qualified Icicle.Common.Base       as V
import qualified Icicle.Core.Eval.Program as PV
import qualified Icicle.Core.Program.Program as P

import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Avalanche.Prim.Eval  as APF
import qualified Icicle.Avalanche.Prim.Flat  as APF
import qualified Icicle.Avalanche.Eval    as AE

import qualified Data.Set as Set


data Partition =
  Partition
    Entity
    Attribute
    [AsAt Value]
  deriving (Eq,Show)

type Result a n = Either (SimulateError a n) ([(OutputName, Value)], Set.Set FactIdentifier)

data SimulateError a n
 = SimulateErrorRuntime (PV.RuntimeError a n)
 | SimulateErrorRuntime' (AE.RuntimeError a n APF.Prim)
 | SimulateErrorCannotConvertToCore        Value ValType
 | SimulateErrorCannotConvertFromCore      V.BaseValue
  deriving (Eq,Show)

instance Pretty n => Pretty (SimulateError a n) where
 pretty (SimulateErrorRuntime e)
  = pretty e
 pretty (SimulateErrorRuntime' e)
  = pretty e
 pretty (SimulateErrorCannotConvertToCore v t)
  = "Cannot convert value to Core: " <> pretty v <+> ":" <+> pretty t
 pretty (SimulateErrorCannotConvertFromCore v)
  = "Cannot convert value from Core: " <> pretty v


streams :: [AsAt Fact] -> [Partition]
streams =
    P.concatMap makePartition
  . fmap       (sortBy (compare `on` atTime))
  . groupBy    ((==) `on` partitionBy)
  . sortBy     (compare `on` partitionBy)


partitionBy :: AsAt Fact -> (Entity, Attribute)
partitionBy f =
  (factEntity . atFact $ f, factAttribute . atFact $ f)


makePartition :: [AsAt Fact] -> [Partition]
makePartition []
 = []
makePartition fs@(f:_)
 = [ Partition  (factEntity    $ atFact f)
                (factAttribute $ atFact f)
                (fmap (\f' -> AsAt (factValue $ atFact f') (atTime f')) fs) ]

evaluateVirtualValue :: (Hashable n, Eq n) => P.Program a n -> Time -> [AsAt Value] -> Result a n
evaluateVirtualValue p t vs
 = do   vs' <- zipWithM toCore [1..] vs

        xv  <- first SimulateErrorRuntime
             $ PV.eval t vs' p

        v'  <- traverse (\(k,v) -> (,) <$> pure k <*> valueFromCore' v) (PV.value xv)
        return (v', PV.history xv)
 where
  toCore n a
   = do v' <- valueToCore' (atFact a) (P.inputType p)
        return $ a { atFact = (B.BubbleGumFact $ B.Flavour n $ atTime a, v') }

evaluateVirtualValue' :: (Hashable n, Eq n, Show n, Show a) => A.Program a n APF.Prim -> Time -> [AsAt Value] -> Result a n
evaluateVirtualValue' p t vs
 = do   vs' <- zipWithM toCore [1..] vs

        (outs,bgs)
            <- first SimulateErrorRuntime'
             $ AE.evalProgram APF.evalPrim t vs' p

        v'  <- traverse (\(k,v) -> (,) <$> pure k <*> valueFromCore' v) outs
        -- bg' <- traverse (traverse valueFromCore') (fst xv)
        return (v', bgs)
 where
  toCore n a
   = do v' <- valueToCore' (atFact a) (A.input p)
        return $ a { atFact = (B.BubbleGumFact $ B.Flavour n $ atTime a, v') }

valueToCore' :: Value -> ValType -> Either (SimulateError a n) BaseValue
valueToCore' v vt
 = maybe (Left (SimulateErrorCannotConvertToCore v vt)) Right (valueToCore v vt)

valueFromCore' :: V.BaseValue -> Either (SimulateError a n) Value
valueFromCore' v
 = maybe (Left (SimulateErrorCannotConvertFromCore v)) Right (valueFromCore v)
