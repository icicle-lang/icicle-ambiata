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



data Exp n
 = XVar (Name n)
 | XApp (Exp n) (Exp n)
 | XPrim Prim
 -- Only really used for arguments passed to primitives such as fold
 | XLam (Name n) ValType (Exp n)
 | XLet (Name n) (Exp n) (Exp n)
 deriving (Eq,Ord,Show)


takeApps :: Exp n -> (Exp n, [Exp n]) 
takeApps xx
 = case xx of
    XApp p q
     -> let (f,as) = takeApps p
        in  (f, mappend as [q])
    _
     -> (xx, [])


takePrimApps :: Exp n -> Maybe (Prim, [Exp n])
takePrimApps xx
 = case takeApps xx of
    (XPrim p, args) -> Just (p, args)
    _               -> Nothing


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


