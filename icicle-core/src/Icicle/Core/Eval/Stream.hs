-- | Simple stream evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wwarn #-}
module Icicle.Core.Eval.Stream (
      StreamValue  (..)
    , StreamHeap
    , RuntimeError (..)
    , eval
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value as V
import              Icicle.Core.Stream
import qualified    Icicle.Common.Exp.Eval  as XV -- eXpression eVal
import qualified    Icicle.Core.Eval.Exp    as XV -- eXpression eVal
import              Icicle.Core.Exp.Prim

import              Icicle.Internal.Pretty

import              P

import qualified    Data.Map as Map
import              Data.List (zip, transpose)
import              Data.Hashable (Hashable)



-- | A stream value is a list of values, but also the list of cookies or bubblegum.
-- This bubblegum is used to track the flow - which values are used by the computation,
-- and specifically which values will be needed by tomorrow's or next computation.
-- For each value, we also keep its date around so it can be windowed.
--
-- We also need to know whether the stream has been windowed: if it is not windowed, any
-- reduction like sum can just start where it left off.
-- If it is windowed, we instead store the values needed to recompute, as values will drop off
-- the start of the window.
data StreamValue
 = StreamValue {
     streamValues :: [BaseValue]
   , streamZero   :: BaseValue
 }


-- | A stream heap maps from names to stream values
type StreamHeap n
 = Map.Map (Name n) StreamValue


-- | Things that can go wrong for stream evaluation
data RuntimeError a n
 = RuntimeErrorExp            (XV.RuntimeError a n Prim)
 | RuntimeErrorNoSuchStream   (Name n)
 | RuntimeErrorExpNotOfType   (V.Value a n Prim) ValType
 | RuntimeErrorExpNotBaseType (V.Value a n Prim)
 deriving (Show, Eq)


instance (Pretty n) => Pretty (RuntimeError a n) where
 pretty (RuntimeErrorExp x)
  = "Expression error:" <> line
  <> indent 2 (pretty x)
 pretty (RuntimeErrorNoSuchStream n)
  = "No such stream variable: " <> pretty n
 pretty (RuntimeErrorExpNotOfType v t)
  = "Expression has wrong type: " <> line
  <> "  Expression:    " <> pretty v <> line
  <> "  Expected type: " <> pretty t
 pretty (RuntimeErrorExpNotBaseType v)
  = "Expression has function type, but should be simple value: " <> line
  <> "  Expression:    " <> pretty v

-- | Evaluate a stream.
-- We take the precomputation environment, the stream of concrete values,
-- and the heap with all preceding streams stored.
eval    :: (Hashable n, Eq n)
        => V.Heap    a n Prim   -- ^ The expression heap with precomputations
        -> StreamHeap       n   -- ^ Any streams that have already been evaluated
        -> Stream         a n   -- ^ Stream to evaluate
        -> Either (RuntimeError a n) (StreamHeap n)
eval xh sh s
 = case s of
    SFold n _ z k
     -> do  let xhZ = Map.map (VBase . streamZero) sh `Map.union` xh
            vz <- evalX xhZ z
            vz' <- V.getBaseValue (RuntimeErrorExpNotBaseType vz) vz
            vs <- foldGo n k vz inputHeaps
            return (Map.insert n (StreamValue vs vz') sh)

    SFilter p ss
     -> do bs <- mapM (evalF p) inputHeaps
           sh' <- foldM (eval xh) (filterHeap bs sh) ss
           return (Map.union sh $ unfilterHeap bs sh')


 where
  foldGo _ _ _ []
   = return []
  foldGo n k vz (h:hs)
   = do let h' = Map.insert n vz $ Map.union h xh
        vk <- evalX h' k
        vs <- foldGo n k vk hs
        vk' <- V.getBaseValue (RuntimeErrorExpNotBaseType vk) vk
        return (vk' : vs)
    

  -- Evaluate a filter, and count facts as "used" if they are inside a window
  evalF p h
   = do res <- evalX (Map.union h xh) p
        val <- V.getBaseValue (RuntimeErrorExpNotBaseType res) res
        return ( val == VBool True )

  filterHeap bs sheep
   = Map.map (\v -> v { streamValues = fmap snd $ filter fst $ zip bs $ streamValues v }) sheep

  unfilterHeap bs sheep
   = Map.map (unfilterStreamValue bs) sheep

  unfilterStreamValue bs v
   = v { streamValues = unfilter bs (streamZero v) (streamValues v) }
  
  unfilter [] _ _
   = []
  unfilter (True : bs) _ (a : as)
   = a : unfilter bs a as
  unfilter (False : bs) a as
   = a : unfilter bs a as

  -- Wrong number of booleans. should not happen
  unfilter _ _ _ = []

  inputHeaps
   = transposeMap $ Map.map (fmap VBase . streamValues) sh

  transposeMap :: Ord k => Map.Map k [v] -> [Map.Map k v]
  transposeMap mm
   = fmap Map.fromList
   $ transpose
   $ fmap (\(k,vs) -> fmap ((,) k) vs)
   $ Map.toList mm


  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX hp x
   = first RuntimeErrorExp
   $ XV.eval XV.evalPrim hp x

