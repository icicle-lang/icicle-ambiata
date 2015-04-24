{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Error (
      CheckError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Exp

import              P

data CheckError n
 -- No such variable
 = CheckErrorVarNotInEnv (Name n)
 -- Application of x1 to x2, types don't match
 | CheckErrorApp (Exp n) (Exp n) Type Type
 -- For simplicity, require all names to be unique.
 -- This removes shadowing complications
 | CheckErrorNameNotUnique (Name n)
 deriving Show

instance (Pretty n) => Pretty (CheckError n) where
 pretty e
  = case e of
    CheckErrorVarNotInEnv n
     -> text "Variable not bound: " <> pretty n
    CheckErrorApp fun arg funt argt
     ->  text "Application types don't fit: "
     <+> indent 4 ( text "Fun: " <> pretty fun
                <+> text "With type: " <> pretty funt
                <+> text "Arg: " <> pretty arg
                <+> text "With type: " <> pretty argt)
    CheckErrorNameNotUnique n
     ->  text "Bound name is not unique: " <> pretty n
     <+> text "(for simplicity, we require all core names to be unique)"
