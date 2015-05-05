-- | Errors that can occur when typechecking an expression
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Error (
      ExpError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Exp
import              Icicle.Core.Exp.Prim

import              P

data ExpError n
 -- No such variable
 = ExpErrorVarNotInEnv (Name n)

 -- Application of x1 to x2, types don't match
 | ExpErrorApp (Exp n) (Exp n) Type Type

 -- For simplicity, require all names to be unique.
 -- This removes shadowing complications
 | ExpErrorNameNotUnique (Name n)

 -- Primitives cannot be partially applied
 | ExpErrorPrimitiveNotFullyApplied Prim (Exp n)
 deriving (Show, Eq, Ord)


instance (Pretty n) => Pretty (ExpError n) where
 pretty e
  = case e of
    ExpErrorVarNotInEnv n
     -> text "Variable not bound: " <> pretty n
    ExpErrorApp fun arg funt argt
     ->  text "Application error:"
     <+> indent 0 ( text "Fun: " <> pretty fun              <> line
                 <> text "With type: " <> pretty funt       <> line
                 <> text "Arg: " <> pretty arg              <> line
                 <> text "With type: " <> pretty argt       <> line)
    ExpErrorNameNotUnique n
     ->  text "Bound name is not unique: " <> pretty n                  <> line
     <>  text "(for simplicity, we require all core names to be unique)"

    ExpErrorPrimitiveNotFullyApplied p x
     ->  text "The primitive " <> pretty p <> text " is not fully applied in expression " <> pretty x

