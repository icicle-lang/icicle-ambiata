{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Stream.Stream (
      Stream          (..)
    , renameStream
    , isStreamWindowed
    , isPredicateWindowed
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp.Compounds
import              Icicle.Core.Exp
import              Icicle.Common.Exp.Exp (renameExp)

import              P


data Stream a n
 = SFold    !(Name n) !ValType !(Exp a n) !(Exp a n)
 | SFilter           !(Exp a n) [Stream a n]
 -- SWindow !ValType !WindowUnit !(Maybe WindowUnit) !(Name n)
 deriving (Eq,Ord,Show)


renameStream :: (Name n -> Name n') -> Stream a n -> Stream a n'
renameStream f (SFold n t x y)        = SFold (f n) t (renameExp f x) (renameExp f y)
renameStream f (SFilter x ss)         = SFilter (renameExp f x) (fmap (renameStream f) ss)


-- | Check if given stream originates from a windowed or not
-- This only checks against the exact windows that Source.ToCore generates.
-- This will need to be changed once the "scan" primitive is added to Source.
isStreamWindowed :: Eq n => [Stream a n] -> Name n -> Bool
isStreamWindowed ss nm
 = go ss
 where
  go [] = False
  go (SFilter x ss' : rest)
   | isPredicateWindowed x
   , containsName ss'
   = True
   | not $ isPredicateWindowed x
   , go ss'
   = True
   | otherwise
   = go rest
  go (SFold{} : rest)
   = go rest

  containsName []
   = False
  containsName (SFilter _ ss' : rest)
   = containsName ss' || containsName rest
  containsName (SFold n _ _ _ : rest)
   = n == nm || containsName rest


-- | Check if filter predicate is a window
isPredicateWindowed :: Exp a n -> Bool
isPredicateWindowed x
 | Just (PrimWindow{}, _) <- takePrimApps x
 = True
 | otherwise
 = False

-- Pretty printing ---------------


instance (Pretty n) => Pretty (Stream a n) where
 pretty (SFold n t z k)
  = "STREAM_FOLD (" <> pretty n <> " : " <> pretty t <> ")" <> line
  <> indent 2 "INIT:" <> line
  <> indent 4 (pretty z) <> line
  <> indent 2 "KONS:" <> line
  <> indent 4 (pretty k)
  <> line
 pretty (SFilter x ss)
  = line
  <> "STREAM_FILTER" <> line
  <> indent 2 "PREDICATE: " <> line
  <> indent 4 (pretty x) <> line
  <> indent 2 "STREAMS:" <> line
  <> indent 4 (vsep $ fmap pretty ss)

