-- | This needs cleaning up but the idea is there
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Simulator (
    streams
  , Partition
  , evaluateVirtuals
  ) where

import           Data.List
import           Data.Either.Combinators
import           Data.Text (Text)

import qualified Icicle.BubbleGum   as B
import           Icicle.Data
import           Icicle.Dictionary

import           P

import qualified Icicle.Core.Eval.Exp     as XV
import qualified Icicle.Core.Eval.Program as PV
import qualified Icicle.Core.Program.Program as P



data Partition =
  Partition
    Entity
    Attribute
    [AsAt Value]
  deriving (Eq,Show)

type Result = Either SimulateError Value

data SimulateError
 = SimulateErrorRuntime (PV.RuntimeError Text)
 | SimulateErrorCannotConvertToCore        Value
 | SimulateErrorCannotConvertFromCore  (XV.Value Text)
  deriving (Eq,Show)


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
        return v'
 where
  toCore a
   = do v' <- valueToCore $ fact a
        return a { fact = (B.BubbleGumFact (B.Flavour 0 (time a)) (Attribute "TODO"), v') }


valueToCore :: Value -> Either SimulateError (XV.Value Text)
valueToCore v
 = case v of
    IntValue i     -> return $ XV.VInt i
    BooleanValue b -> return $ XV.VBool b
    ListValue (List ls)
                   -> XV.VArray
                            <$> mapM valueToCore ls
    _              -> Left   $ SimulateErrorCannotConvertToCore v


valueFromCore :: XV.Value Text -> Either SimulateError Value
valueFromCore v
 = case v of
    XV.VInt i      -> return $ IntValue i
    XV.VBool b     -> return $ BooleanValue b
    XV.VArray vs   -> ListValue . List
                  <$> mapM valueFromCore vs
    _              -> Left   $ SimulateErrorCannotConvertFromCore v

