-- | Definition of expressions
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Exp (
      Exp(..)
    , Ann
    , renameExp
    , annotOfExp
    , TransformX(..)
    ) where

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.NanEq
import           Icicle.Common.Type
import           Icicle.Internal.Pretty

import           P

import           Data.Set (Set)


-- | Incredibly simple expressions;
data Exp a n p
 -- | Read a variable from heap
 = XVar !a !(Name n)

 -- | A predefined primitive
 | XPrim !a !p

 -- | A constant simple value
 -- We need the type here because if this is, say, an empty array,
 -- we would not be able to guess the element type.
 | XValue !a !ValType !BaseValue

 -- | Apply something
 | XApp !a !(Exp a n p) !(Exp a n p)

 -- | Lambda abstraction:
 -- This is only really used for arguments passed to primitives such as fold.
 | XLam !a !(Name n) !ValType !(Exp a n p)

 -- | Let binding
 | XLet !a !(Name n) !(Exp a n p) !(Exp a n p)
 deriving (Eq, Ord, Show, Generic)

-- Turns out making the whole AST strict has a massive impact
-- on compilation performance.
instance (NFData a, NFData n, NFData p) => NFData (Exp a n p)

instance (NanEq a, NanEq n, NanEq p) => NanEq (Exp a n p)

renameExp :: (Name n -> Name n') -> Exp a n p -> Exp a n' p
renameExp f (XVar a n)     = XVar a (f n)
renameExp _ (XPrim a p)    = XPrim a p
renameExp _ (XValue a t v) = XValue a t v
renameExp f (XApp a p q)   = XApp a (renameExp f p) (renameExp f q)
renameExp f (XLam a n t b) = XLam a (f n) t (renameExp f b)
renameExp f (XLet a n p q) = XLet a (f n) (renameExp f p) (renameExp f q)


annotOfExp :: Exp a n p -> a
annotOfExp (XVar   a _)     = a
annotOfExp (XPrim  a _)     = a
annotOfExp (XValue a _ _)   = a
annotOfExp (XApp   a _ _)   = a
annotOfExp (XLam   a _ _ _) = a
annotOfExp (XLet   a _ _ _) = a


class TransformX x where
 transformX :: Monad m
            => (Name  n   -> m (Name   n'))
            -> (Exp a n p -> m (Exp a' n' p'))
            ->    x a n p -> m (x   a' n' p')

type Ann a n = (a, Set (Name n))



-- Pretty printing ---------------

--
-- FIXME Annoyingly 'takeLets' and 'takeApps' are also in
-- FIXME Icicle.Common.Exp.Compounds. We can't use them though because the
-- FIXME Pretty instance must be defined here. Ideally pretty printing wouldn't
-- FIXME be done using a type class.
--

takeLams :: Exp a n p -> ([(Name n, ValType)], Exp a n p)
takeLams = \case
  XLam _ n t x ->
    let
      (nts, tl) =
        takeLams x
    in
      ((n, t) : nts, tl)
  tl ->
    ([], tl)

takeLets :: Exp a n p -> ([(Name n, Exp a n p)], Exp a n p)
takeLets = \case
  XLet _ n x i ->
    let
      (nxs, tl) =
        takeLets i
    in
      ((n, x) : nxs, tl)
  tl ->
    ([], tl)

takeApps :: Exp a n p -> (Exp a n p, [Exp a n p])
takeApps = \case
  XApp _ fun0 arg ->
    let
      (fun, args) =
        takeApps fun0
    in
      (fun, args <> [arg])

  x ->
    (x, [])

instance (Pretty n, Pretty p) => Pretty (Exp a n p) where
  prettyPrec p xx =
    case xx of
      XVar _ n ->
        annotate AnnVariable $
          prettyPrec p n

      XPrim _ x ->
        annotate AnnPrimitive $
          prettyPrec p x

      XValue _ _ v ->
        annotate AnnConstant $
          prettyPrec p v

      XLam{} ->
        let
          (nts, tl) =
            takeLams xx

          ppBind (n, _t) =
            annotate AnnBinding (pretty n)
        in
          hang 2 . parensWhenArg p $
            prettyPunctuation "\\" <>
            sep [
                hsep (fmap ppBind nts) <+> prettyPunctuation "->"
              , pretty tl
              ]

      XLet{} ->
        let
          (nxs, tl) =
            takeLets xx

          ppBind (n, x) =
            prettyBinding (pretty n) (pretty x)
        in
          parensWhenArg p . vsep $ [
              prettyKeyword "let"
            , indent 2 . vsep . punctuate line $ fmap ppBind nxs
            , prettyKeyword "in"
            , indent 2 $ pretty tl
            ]

      XApp{} ->
        let
          (fun, args) =
            takeApps xx
        in
          prettyApp sep p fun args
