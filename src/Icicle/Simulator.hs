-- | This needs cleaning up but the idea is there
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Simulator (
    streams
  , Partition(..)
  , SimulateError
  , evaluateVirtuals
  , evaluateVirtualValue
  , valueToCore
  , valueFromCore
  ) where

import           Data.List
import           Data.Either.Combinators
import qualified Data.Map           as Map
import           Data.Text (Text)

import qualified Icicle.BubbleGum   as B
import           Icicle.Data
import           Icicle.Data.DateTime
import           Icicle.Dictionary

import           Icicle.Internal.Pretty

import           P

import qualified Icicle.Common.Base       as V
import qualified Icicle.Core.Eval.Program as PV
import qualified Icicle.Core.Program.Program as P



data Partition =
  Partition
    Entity
    Attribute
    [AsAt Value]
  deriving (Eq,Show)

type Result = Either SimulateError (Value, [B.BubbleGumOutput Text Value])

data SimulateError
 = SimulateErrorRuntime (PV.RuntimeError Text)
 | SimulateErrorCannotConvertToCore        Value
 | SimulateErrorCannotConvertFromCore      V.BaseValue
  deriving (Eq,Show)

instance Pretty SimulateError where
 pretty (SimulateErrorRuntime e)
  = pretty e
 pretty (SimulateErrorCannotConvertToCore v)
  = "Cannot convert value to Core: " <> pretty v
 pretty (SimulateErrorCannotConvertFromCore v)
  = "Cannot convert value from Core: " <> pretty v


streams :: [AsAt Fact] -> [Partition]
streams =
    P.concatMap makePartition
  . fmap       (sortBy (compare `on` time))
  . groupBy    ((==) `on` partitionBy)
  . sortBy     (compare `on` partitionBy)


partitionBy :: AsAt Fact -> (Entity, Attribute)
partitionBy f =
  (entity . fact $ f, attribute . fact $ f)


makePartition :: [AsAt Fact] -> [Partition]
makePartition []
 = []
makePartition fs@(f:_)
 = [ Partition  (entity    $ fact f)
                (attribute $ fact f)
                (fmap (\f' -> AsAt (value $ fact f') (time f')) fs) ]


evaluateVirtuals :: Dictionary -> DateTime -> [Partition] -> [(Attribute, [(Entity, Result)])]
evaluateVirtuals (Dictionary fields) date facts
 = P.concatMap go fields
 where
  go (attr, VirtualDefinition virt)
   = [(attr, evaluateVirtual virt date facts)]
  go _
   = []

evaluateVirtual  :: Virtual -> DateTime -> [Partition] -> [(Entity, Result)]
evaluateVirtual virt date facts
 = P.concatMap go facts
 where
  go (Partition ent attr values)
   | attr == concrete virt
   = [(ent, evaluateVirtualValue (program virt) date values)]
   | otherwise
   = []

evaluateVirtualValue :: P.Program Text -> DateTime -> [AsAt Value] -> Result
evaluateVirtualValue p date vs
 = do   vs' <- mapM toCore vs

        xv  <- mapLeft SimulateErrorRuntime
             $ PV.eval date vs' p

        v'  <- valueFromCore $ PV.value xv
        bg' <- mapM (B.mapValue valueFromCore) (PV.history xv)
        return (v', bg')
 where
  toCore a
   = do v' <- valueToCore $ fact a
        return a { fact = (B.BubbleGumFact $ B.Flavour 0 $ time a, v') }


valueToCore :: Value -> Either SimulateError V.BaseValue
valueToCore v
 = case v of
    IntValue i     -> return $ V.VInt i
    BooleanValue b -> return $ V.VBool b
    ListValue (List ls)
                   -> V.VArray
                            <$> mapM valueToCore ls

    PairValue a b  -> V.VPair <$> valueToCore a <*> valueToCore b

    MapValue  kvs  -> V.VMap . Map.fromList
                            <$> mapM (\(a,b) -> (,) <$> valueToCore a <*> valueToCore b) kvs

    _              -> Left   $ SimulateErrorCannotConvertToCore v


valueFromCore :: V.BaseValue -> Either SimulateError Value
valueFromCore v
 = case v of
    V.VInt i      -> return $ IntValue i
    V.VUnit       -> return $ IntValue 13013
    V.VBool b     -> return $ BooleanValue b
    V.VDateTime d -> return $ DateValue $ Date $ renderDate d
    V.VArray vs   -> ListValue . List
                  <$> mapM valueFromCore vs
    V.VPair a b   -> PairValue <$> valueFromCore a <*> valueFromCore b
    V.VSome a     -> valueFromCore a
    V.VNone       -> return Tombstone
    V.VMap vs     -> MapValue
                  <$> mapM (\(a,b) -> (,) <$> valueFromCore a <*> valueFromCore b) (Map.toList vs)


