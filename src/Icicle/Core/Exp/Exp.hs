{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Exp (
      Exp     (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Prim

import              P



data Exp n
 = XVar (Name n)
 | XApp (Exp n) (Exp n)
 | XPrim Prim
 | XLam (Name n) ValType (Exp n)
 | XLet (Name n) (Exp n) (Exp n)
 deriving (Eq,Ord,Show)



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
 pretty (XLet b x i) = text "let " <> pretty b <> text " = " <> pretty x <> text " in " <> pretty i


