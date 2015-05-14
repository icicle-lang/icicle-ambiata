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
data Exp n p
 -- | Read a variable from heap
 = XVar (Name n)

 -- | A predefined primitive
 | XPrim p

 -- | A constant simple value
 -- We need the type here because if this is, say, an empty array,
 -- we would not be able to guess the element type.
 | XValue ValType BaseValue

 -- | Apply something
 | XApp (Exp n p) (Exp n p)

 -- | Lambda abstraction:
 -- This is only really used for arguments passed to primitives such as fold.
 | XLam (Name n) ValType (Exp n p)

 -- | Let binding
 | XLet (Name n) (Exp n p) (Exp n p)
 deriving (Eq,Ord,Show)


renameExp :: (Name n -> Name n') -> Exp n p -> Exp n' p
renameExp f (XVar n)     = XVar (f n)
renameExp _ (XPrim p)    = XPrim p
renameExp _ (XValue t v) = XValue t v
renameExp f (XApp p q)   = XApp (renameExp f p) (renameExp f q)
renameExp f (XLam n t b) = XLam (f n) t (renameExp f b)
renameExp f (XLet n p q) = XLet (f n) (renameExp f p) (renameExp f q)

class TransformX x where
 transformX :: (Applicative m, Monad m) => (Name n -> m (Name n')) -> (Exp n p -> m (Exp n' p')) -> x n p -> m (x n' p')

-- Pretty printing ---------------

instance (Pretty n, Pretty p) => Pretty (Exp n p) where
 pretty (XVar n)    = pretty n
 pretty (XPrim p)   = pretty p
 pretty (XValue t v)= pretty v <+> text ":" <+> pretty t

 pretty (XApp p q)  = inner' p <+> inner q
  where
   inner i
    = case i of
       XApp{}   -> parens $ pretty i
       XLam{}   -> parens $ pretty i
       XValue{} -> parens $ pretty i
       _        ->          pretty i
   inner' i
    = case i of
       XLam{} -> parens $ pretty i
       _      ->          pretty i

 pretty (XLam b t x) = text "\\" <> pretty b <> text " : " <> pretty t <> text ". " <> align (pretty x)

 pretty (XLet b x i) = text "let " <> pretty b
                    <> text " = "  <> align (pretty x)
                   </> text " in " <> align (pretty i)



