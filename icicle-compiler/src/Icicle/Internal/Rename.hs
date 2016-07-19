-- | Renaming all the different ASTs in Icicle.
--
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Internal.Rename where

import           Icicle.Common.Base
import           Icicle.Common.Exp
import qualified Icicle.Core.Exp             as CE
import qualified Icicle.Source.Query         as SQ

import           Control.Arrow ((***))

import           Data.Hashable (Hashable)

import           P

renameQT :: Hashable m => (n -> m) -> SQ.QueryTop a n -> SQ.QueryTop a m
renameQT f (SQ.QueryTop x queryName q)
  = SQ.QueryTop (renameN f x) queryName (renameQ f q)

renameQ :: Hashable m => (n -> m) -> SQ.Query a n -> SQ.Query a m
renameQ f (SQ.Query cs x)
  = SQ.Query (fmap (renameC f) cs) (renameX f x)

renameC :: Hashable m => (n -> m) -> SQ.Context a n -> SQ.Context a m
renameC f cc = case cc of
  SQ.Windowed  a b c   -> SQ.Windowed  a b c
  SQ.Latest    a b     -> SQ.Latest    a b
  SQ.GroupBy   a e     -> SQ.GroupBy   a (renameX f e)
  SQ.GroupFold a k v e -> SQ.GroupFold a (renameN f k) (renameN f v) (renameX f e)
  SQ.Distinct  a e     -> SQ.Distinct  a (renameX f e)
  SQ.Filter    a e     -> SQ.Filter    a (renameX f e)
  SQ.LetFold   a x     -> SQ.LetFold   a (renameF f x)
  SQ.Let       a n e   -> SQ.Let       a (renameN f n) (renameX f e)

renameF :: Hashable m => (n -> m) -> SQ.Fold (SQ.Query a n) a n -> SQ.Fold (SQ.Query a m) a m
renameF f d
  = SQ.Fold
    (renameN f $ SQ.foldBind d)
    (renameX f $ SQ.foldInit d)
    (renameX f $ SQ.foldWork d)
    (SQ.foldType d)

renameX :: Hashable m => (n -> m) -> SQ.Exp a n -> SQ.Exp a m
renameX f e = case e of
  SQ.Var a n     -> SQ.Var a (renameN f n)
  SQ.Nested a q  -> SQ.Nested a (renameQ f q)
  SQ.App a e1 e2 -> SQ.App a (renameX f e1) (renameX f e2)
  SQ.Prim a p    -> SQ.Prim a p
  SQ.Case a scrut pats
   -> SQ.Case a (renameX f scrut)
    $ fmap (renamePat f *** renameX f) pats

renamePat :: Hashable m => (n -> m) -> SQ.Pattern n -> SQ.Pattern m
renamePat f (SQ.PatCon c ps)   = SQ.PatCon c $ fmap (renamePat f) ps
renamePat _  SQ.PatDefault     = SQ.PatDefault
renamePat f (SQ.PatVariable n) = SQ.PatVariable $ renameN f n

renameN :: Hashable m => (n -> m) -> Name n -> Name m
renameN f n = nameOf (renameNameBase f (nameBase n))

renameNameBase :: (n -> m) -> NameBase n -> NameBase m
renameNameBase f (NameBase v)     = NameBase (f v)
renameNameBase f (NameMod  n1 n2) = NameMod  (f n1) (renameNameBase f n2)

renameNX :: Hashable m => (n -> m) -> (Name n, CE.Exp a n) -> (Name m, CE.Exp a m)
renameNX f (n, e) = (renameN f n, renameCX f e)

renameCX :: Hashable m => (n -> m) -> CE.Exp a n -> CE.Exp a m
renameCX f e = case e of
  XVar   a n       -> XVar   a (renameN f n)
  XApp   a e1 e2   -> XApp   a (renameCX f e1) (renameCX f e2)
  XLam   a n t  e1 -> XLam   a (renameN f n) t (renameCX f e1)
  XLet   a n e1 e2 -> XLet   a (renameN f n) (renameCX f e1) (renameCX f e2)
  XPrim  a p       -> XPrim  a p
  XValue a x y     -> XValue a x y

