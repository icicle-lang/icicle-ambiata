-- | Definition of expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Exp (
      Exp     (..)
    , takeApps
    , takePrimApps
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Prim

import              P


-- | Incredibly simple expressions;
data Exp n
 -- | Read a variable from heap
 = XVar (Name n)

 -- | Apply something
 | XApp (Exp n) (Exp n)

 -- | A predefined primitive
 | XPrim Prim

 -- | Lambda abstraction:
 -- This is only really used for arguments passed to primitives such as fold.
 | XLam (Name n) ValType (Exp n)

 -- | Let binding
 | XLet (Name n) (Exp n) (Exp n)
 deriving (Eq,Ord,Show)


-- | Split an expression into its function part and any arguments applied to it.
-- If it's not an application, arguments will be empty.
takeApps :: Exp n -> (Exp n, [Exp n]) 
takeApps xx
 = case xx of
    XApp p q
     -> let (f,as) = takeApps p
        in  (f, mappend as [q])
    _
     -> (xx, [])


-- | Check if an expression is a primitive application
takePrimApps :: Exp n -> Maybe (Prim, [Exp n])
takePrimApps xx
 = case takeApps xx of
    (XPrim p, args) -> Just (p, args)
    _               -> Nothing


instance Rename Exp where
 rename f (XVar n) = XVar (f n)
 rename f (XApp p q) = XApp (rename f p) (rename f q)
 rename _ (XPrim p) = XPrim p
 rename f (XLam n t b) = XLam (f n) t (rename f b)
 rename f (XLet n p q) = XLet (f n) (rename f p) (rename f q)

-- Pretty printing ---------------

instance (Pretty n) => Pretty (Exp n) where
 pretty (XVar n) = pretty n

 pretty (XApp p q) = pretty p <+> inner q
  where
   inner i
    = case i of
       XApp{} -> parens $ pretty i
       XLam{} -> parens $ pretty i
       _      ->          pretty i

 pretty (XPrim p)  = pretty p
 pretty (XLam b t x) = text "\\" <> pretty b <> text " : " <> pretty t <> text ". " <> pretty x
 pretty (XLet b x i) = text "let " <> pretty b
                    <> text " = "  <> align (pretty x)
                   </> text " in " <> align (pretty i)


