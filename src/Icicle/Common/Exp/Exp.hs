-- | Definition of expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Exp (
      Exp     (..)
    , renameExp
    , TransformX (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type

import              P


-- | Incredibly simple expressions;
data Exp a n p
 -- | Read a variable from heap
 = XVar a (Name n)

 -- | A predefined primitive
 | XPrim a p

 -- | A constant simple value
 -- We need the type here because if this is, say, an empty array,
 -- we would not be able to guess the element type.
 | XValue a ValType BaseValue

 -- | Apply something
 | XApp a (Exp a n p) (Exp a n p)

 -- | Lambda abstraction:
 -- This is only really used for arguments passed to primitives such as fold.
 | XLam a (Name n) ValType (Exp a n p)

 -- | Let binding
 | XLet a (Name n) (Exp a n p) (Exp a n p)
 deriving (Eq,Ord,Show)


renameExp :: (Name n -> Name n') -> Exp a n p -> Exp a n' p
renameExp f (XVar a n)     = XVar a (f n)
renameExp _ (XPrim a p)    = XPrim a p
renameExp _ (XValue a t v) = XValue a t v
renameExp f (XApp a p q)   = XApp a (renameExp f p) (renameExp f q)
renameExp f (XLam a n t b) = XLam a (f n) t (renameExp f b)
renameExp f (XLet a n p q) = XLet a (f n) (renameExp f p) (renameExp f q)

class TransformX x where
 transformX :: (Applicative m, Monad m)
            => (Name  n   -> m (Name   n'))
            -> (Exp a n p -> m (Exp a' n' p'))
            ->    x a n p -> m (x   a' n' p')

-- Pretty printing ---------------

instance (Pretty n, Pretty p) => Pretty (Exp a n p) where
 pretty (XVar _ n)    = pretty n
 pretty (XPrim _ p)   = pretty p
 pretty (XValue _ t v)= pretty v <+> text ":" <+> pretty t

 pretty (XApp _ p q)  = align (inner' p </> inner q)
  where
   inner i
    = case i of
       XApp{}   -> parens $ pretty i
       XLam{}   -> parens $ pretty i
       XValue{} -> parens $ pretty i
       XLet{}   -> parens $ pretty i
       _        ->          pretty i
   inner' i
    = case i of
       XLam{} -> parens $ pretty i
       XLet{} -> parens $ pretty i
       _      ->          pretty i

 pretty (XLam _ b t x) = line <> text "\\" <> pretty b <> text " : " <> pretty t <> text ". " </> pretty x

 pretty (XLet _ b x i) = line
                      <> text "let " <> pretty b
                      <> text " = "  <> align (pretty x)
                      <> line
                      <> text " in " <> pretty i



