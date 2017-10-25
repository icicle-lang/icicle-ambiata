{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Canyon.FromAvalanche (
    fromAvalanche
  ) where

import qualified Icicle.Avalanche.Program             as Avalanche
import qualified Icicle.Avalanche.Statement.Statement as Avalanche
import           Icicle.Avalanche.Prim.Flat

import           Icicle.Canyon.Program
import           Icicle.Canyon.Statement

import           Icicle.Common.Base
import           Icicle.Common.Annot
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           P

import           Data.Hashable

fromAvalanche :: Hashable n => n -> n -> a -> Avalanche.Program (Annot a) n Prim -> Program (Annot a) n
fromAvalanche n_let n_acc a_fresh p = Program
 { input      = Avalanche.input      p
 , bindtime   = Avalanche.bindtime   p
 , maxMapSize = Avalanche.maxMapSize p
 , statements = fromAvalancheStatement 
   (FromOptions
      (modName n_let)
      (modName n_acc)
      (\t _ -> Annot (FunT [] t) a_fresh))
   $ Avalanche.statements p
 }

data FromOptions a n n'
 = FromOptions
 { prefixLet :: Name n -> Name n'
 , prefixAccumulator :: Name n -> Name n'
 , annotationOfName :: ValType -> Name n' -> Annot a
 }

fromAvalancheStatement :: FromOptions a n n' -> Avalanche.Statement (Annot a) n Prim -> Statement (Annot a) n'
fromAvalancheStatement info = \case
  Avalanche.Let n x s
   -> Let (prefixLet info n) (functionReturns $ annType $ annotOfExp x) (renameX x)
    $ go s

  Avalanche.If e a b
   -> If (renameX e) (go a) (go b)

  Avalanche.While mode counter t e s
   -> While mode (prefixLet info counter) t (renameX e)
    $ go s
  
  Avalanche.ForeachInts mode counter from to s
   -> ForeachInts mode (prefixLet info counter) (renameX from) (renameX to)
    $ go s

  Avalanche.ForeachFacts (Avalanche.FactBinds ntime ns) t s
   -> ForeachFacts
    (Avalanche.FactBinds
      (prefixLet info ntime)
      (fmap (first $ prefixLet info) ns)) t
    $ go s

  Avalanche.Block ss
   -> Block 
    $ fmap go ss

  Avalanche.InitAccumulator (Avalanche.Accumulator n t e) s
   -> Let (prefixAccumulator info n) t (renameX e)
    $ go s

  Avalanche.Read local acc ty s
   -> Let (prefixLet info local) ty (expOfAccumulator ty $ prefixAccumulator info acc)
    $ go s

  Avalanche.Write n e
   -> Write (prefixAccumulator info n) (renameX e)

  Avalanche.Output a b xts
   -> Output a b
    $ fmap (first renameX) xts

  Avalanche.LoadResumable n t
   -> LoadResumable (prefixLet info n) t

  Avalanche.SaveResumable n t
   -> SaveResumable (prefixLet info n) t

 where
  go = fromAvalancheStatement info

  renameX = renameExp $ prefixLet info

  expOfAccumulator t n = XVar (annotationOfName info t n) n

