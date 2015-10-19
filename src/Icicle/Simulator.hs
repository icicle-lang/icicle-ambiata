-- | This needs cleaning up but the idea is there
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Simulator (
    streams
  , Partition(..)
  , SimulateError
  , evaluateVirtualValue
  , evaluateVirtualValue'
  , valueToCore
  , valueFromCore
  ) where

import           Data.List
import           Data.Either.Combinators
import qualified Data.Map           as Map
import           Data.Text (Text)

import qualified Icicle.BubbleGum   as B
import           Icicle.Common.Base
import           Icicle.Data

import           Icicle.Internal.Pretty

import           P

import qualified Icicle.Common.Base       as V
import qualified Icicle.Core.Eval.Program as PV
import qualified Icicle.Core.Program.Program as P

import qualified Icicle.Avalanche.Program as A
import qualified Icicle.Core.Eval.Exp     as XV
import qualified Icicle.Core.Exp.Prim     as XP
import qualified Icicle.Avalanche.Eval    as AE

data Partition =
  Partition
    Entity
    Attribute
    [AsAt Value]
  deriving (Eq,Show)

type Result a = Either (SimulateError a) ([(OutputName, Value)], [B.BubbleGumOutput Text Value])

data SimulateError a
 = SimulateErrorRuntime (PV.RuntimeError a Text)
 | SimulateErrorRuntime' (AE.RuntimeError a Text XP.Prim)
 | SimulateErrorCannotConvertToCore        Value
 | SimulateErrorCannotConvertFromCore      V.BaseValue
  deriving (Eq,Show)

instance Pretty (SimulateError a) where
 pretty (SimulateErrorRuntime e)
  = pretty e
 pretty (SimulateErrorRuntime' e)
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

evaluateVirtualValue :: P.Program a Text -> DateTime -> [AsAt Value] -> Result a
evaluateVirtualValue p date vs
 = do   vs' <- zipWithM toCore [1..] vs

        xv  <- mapLeft SimulateErrorRuntime
             $ PV.eval date vs' p

        v'  <- mapM (\(n,v) -> (,) n <$> valueFromCore v) $ PV.value xv
        bg' <- mapM (B.mapValue valueFromCore) (PV.history xv)
        return (v', bg')
 where
  toCore n a
   = do v' <- wrapValue <$> (valueToCore $ fact a)
        return a { fact = (B.BubbleGumFact $ B.Flavour n $ time a, v') }

evaluateVirtualValue' :: A.Program a Text XP.Prim -> DateTime -> [AsAt Value] -> Result a
evaluateVirtualValue' p date vs
 = do   vs' <- zipWithM toCore [1..] vs

        xv  <- mapLeft SimulateErrorRuntime'
             $ AE.evalProgram XV.evalPrim date vs' p

        v'  <- mapM (\(n,v) -> (,) n <$> valueFromCore v) $ snd xv
        bg' <- mapM (B.mapValue valueFromCore) (fst xv)
        return (v', bg')
 where
  toCore n a
   = do v' <- valueToCore $ fact a
        return a { fact = (B.BubbleGumFact $ B.Flavour n $ time a, v') }

wrapValue :: V.BaseValue -> V.BaseValue
wrapValue v
 = if   v == V.VError V.ExceptTombstone
   then V.VLeft v
   else V.VRight v

valueToCore :: Value -> Either (SimulateError a) V.BaseValue
valueToCore v
 = case v of
    IntValue i     -> return $ V.VInt i
    DoubleValue d  -> return $ V.VDouble d
    BooleanValue b -> return $ V.VBool b
    ListValue (List ls)
                   -> V.VArray
                            <$> mapM valueToCore ls

    PairValue a b  -> V.VPair <$> valueToCore a <*> valueToCore b

    MapValue  kvs  -> V.VMap . Map.fromList
                            <$> mapM (\(a,b) -> (,) <$> valueToCore a <*> valueToCore b) kvs
    StringValue t  -> return $ V.VString t
    StructValue (Struct vs)
                   -> V.VStruct . Map.fromList
                  <$> mapM (\(a,b) -> (,) <$> pure (V.StructField $ getAttribute a) <*> valueToCore b) vs
    DateValue d    -> return $ V.VDateTime d
    Tombstone      -> return $ V.VError V.ExceptTombstone

valueFromCore :: V.BaseValue -> Either (SimulateError a) Value
valueFromCore v
 = case v of
    V.VInt i      -> return $ IntValue i
    V.VDouble d   -> return $ DoubleValue d
    V.VUnit       -> return $ IntValue 13013
    V.VBool b     -> return $ BooleanValue b
    V.VDateTime d -> return $ DateValue d
    V.VString t   -> return $ StringValue t
    V.VArray vs   -> ListValue . List
                  <$> mapM valueFromCore vs
    V.VPair a b   -> PairValue <$> valueFromCore a <*> valueFromCore b
    V.VSome a     -> valueFromCore a
    V.VNone       -> return Tombstone
    V.VMap vs     -> MapValue
                  <$> mapM (\(a,b) -> (,) <$> valueFromCore a <*> valueFromCore b) (Map.toList vs)
    V.VStruct vs  -> StructValue . Struct <$> mapM (\(a,b) -> (,) <$> pure (Attribute $ V.nameOfStructField a) <*> valueFromCore b) (Map.toList vs)

    V.VError _    -> return Tombstone

    -- TODO need an efficient representation of circular buffer
    V.VBuf _ vs   ->  ListValue . List
                  <$> mapM valueFromCore vs

    -- TODO XXX for now just unwrap the Either constructors.
    -- This is somewhat OK if it is an "Either Error actualvalue"
    V.VLeft  a    -> valueFromCore a
    V.VRight a    -> valueFromCore a

