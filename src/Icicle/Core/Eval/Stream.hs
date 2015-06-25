-- | Simple stream evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Stream (
      StreamValue
    , InitialStreamValue
    , StreamHeap 
    , StreamWindow (..)
    , RuntimeError (..)
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value as V
import              Icicle.Core.Stream
import qualified    Icicle.Common.Exp.Eval  as XV -- eXpression eVal
import qualified    Icicle.Core.Eval.Exp    as XV -- eXpression eVal
import              Icicle.Core.Exp.Prim

import              Icicle.Data
import              Icicle.Data.DateTime

import              Icicle.Internal.Pretty

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map



-- | A stream value is a list of values, but also the list of cookies or bubblegum.
-- This bubblegum is used to track the flow - which values are used by the computation,
-- and specifically which values will be needed by tomorrow's or next computation.
-- For each value, we also keep its date around so it can be windowed.
--
-- We also need to know whether the stream has been windowed: if it is not windowed, any
-- reduction like sum can just start where it left off.
-- If it is windowed, we instead store the values needed to recompute, as values will drop off
-- the start of the window.
type StreamValue
 = ([AsAt (BubbleGumFact, BaseValue)], StreamWindow)

-- | Whether this stream is the result - directly or indirectly - of a windowed source
data StreamWindow = Windowed Int | UnWindowed
 deriving (Eq,Ord,Show)

-- | Right at the start, we need dates on the stream values.
-- These can be used by windowing functions or ignored.
-- Afterwards they are thrown away, but could still be included in the value itself.
type InitialStreamValue
 = [AsAt (BubbleGumFact, BaseValue)]


-- | A stream heap maps from names to stream values
type StreamHeap n
 = Map.Map (Name n) StreamValue


-- | Things that can go wrong for stream evaluation
data RuntimeError n
 = RuntimeErrorExp          (XV.RuntimeError n Prim)
 | RuntimeErrorNoSuchStream (Name n)
 | RuntimeErrorExpNotOfType (V.Value n Prim) ValType
 | RuntimeErrorExpNotBaseType (V.Value n Prim)
 deriving (Show, Eq)


instance (Pretty n) => Pretty (RuntimeError n) where
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
eval    :: Ord n
        => DateTime             -- ^ Snapshot date for checking against windows
        -> V.Heap      n Prim   -- ^ The expression heap with precomputations
        -> InitialStreamValue   -- ^ Concrete inputs start with dates attached
        -> StreamHeap       n   -- ^ Any streams that have already been evaluated
        -> Stream           n   -- ^ Stream to evaluate
        -> Either (RuntimeError n) StreamValue
eval window_check xh concreteValues sh s
 = case s of
    -- Raw input is easy
    Source
     -> return (fmap streamvalue concreteValues, UnWindowed)

    -- Windowed input.
    -- The dates are assured to be increasing, so we could really use a takeWhile.dropWhile or something
    SWindow _ newerThan olderThan n
     -> do  sv <- getInput n
            newer  <- evalX newerThan
            older  <- mapM evalX olderThan

            let windowBy p wind =
                 return ( filter (\v -> p (daysDifference (time v) window_check))
                        $ fst sv
                        , Windowed wind)

            case (newer, older) of
             (VBase (VInt newer'), Nothing)
              -> windowBy (<= newer') newer'
             (VBase (VInt newer'), Just (VBase (VInt older')))
              -> windowBy (\d -> d <= newer' && d >= older') newer'
             (VBase (VInt _), Just older')
              -> Left $ RuntimeErrorExpNotOfType older' IntT
             _
              -> Left $ RuntimeErrorExpNotOfType newer IntT

    -- Transformers are slightly more involved
    -- Evaluate transform over given values.
    -- We don't need the stream heap any more.
    --
    -- The stream values still have their bubblegum attached
    -- but that doesn't really matter here.
    -- We just need to dig down into the expression without
    -- losing the bubblegum.
    STrans (SFilter _) x n
     -> do  -- First get the input source of the transformer
            sv  <- getInput n
            -- Filter according to the actual value, not the date or other junk
            sv' <- filterM (evalFilt x . snd . fact) (fst sv)
            -- Transformers preserve windows
            return (sv', snd sv)

    STrans (SMap _ _) x n
     -> do  -- First get the input source of the transformer
            sv  <- getInput n
            -- Apply the expression to each value
            sv' <- mapM    (applySnd x) (fst sv)
            return (sv', snd sv)


 where
  getInput n
   = maybeToRight   (RuntimeErrorNoSuchStream n)
                    (Map.lookup n sh)


  streamvalue v
   = v { fact = (fst $ fact v, VPair (snd $ fact v) (VDateTime $ time v)) }

  -- Apply x to arg, if it's a bool we know whether to filter
  evalFilt x arg
   = do v <- applyX x arg
        case v of
         V.VBase (VBool b)
          -> return b
         _
          -> Left (RuntimeErrorExpNotOfType v BoolT)

  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX
   = mapLeft RuntimeErrorExp
   . XV.eval XV.evalPrim xh

  -- Apply an expression to a value.
  -- First we need to evaluate the function to a value,
  -- then we can use XV.applyValues
  applyX fX argV
   = do fV <- evalX fX
        mapLeft RuntimeErrorExp
            $ XV.applyValues XV.evalPrim fV (V.VBase argV)

  -- Apply an expression to a stream value, keeping the bubblegum intact
  applySnd fX f
   = do v' <- applyX fX $ snd $ fact f
        case v' of
         V.VBase v'' -> return (f { fact = (fst $ fact f, v'') })
         V.VFun{}    -> Left (RuntimeErrorExpNotBaseType v')

