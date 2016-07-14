-- | Errors that can occur when typechecking an expression
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Error (
      ExpError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp.Exp

import              P


data ExpError a n p
 -- No such variable
 = ExpErrorVarNotInEnv !(Name n)

 -- Application of x1 to x2, types don't match
 | ExpErrorApp !(Exp a n p) !(Exp a n p) !Type !Type

 -- For simplicity, require all names to be unique.
 -- This removes shadowing complications
 | ExpErrorNameNotUnique !(Name n)

 -- Primitives cannot be partially applied
 | ExpErrorPrimitiveNotFullyApplied !p !(Exp a n p)

 -- Primitives cannot be partially applied
 | ExpErrorLambdaNotAllowedHere !(Exp a n p)

 -- Value not of type
 | ExpErrorValueNotOfType !BaseValue !ValType
 deriving (Show, Eq, Ord)

instance NFData (ExpError a n p) where rnf x = seq x ()

instance (Pretty n, Pretty p) => Pretty (ExpError a n p) where
 pretty e
  = case e of
    ExpErrorVarNotInEnv n
     -> text "Variable not bound: " <> pretty n
    ExpErrorApp fun arg funt argt
     ->  text "Application error:"
     <> line
     <>  indent 2 ( text "Fun: " <> pretty fun              <> line
                 <> text "With type: " <> pretty funt       <> line
                 <> text "Arg: " <> pretty arg              <> line
                 <> text "With type: " <> pretty argt       <> line)
    ExpErrorNameNotUnique n
     ->  text "Bound name is not unique: " <> pretty n                  <> line
     <>  text "(for simplicity, we require all core names to be unique)"

    ExpErrorPrimitiveNotFullyApplied p x
     ->  text "The primitive " <> pretty p <> text " is not fully applied in expression " <> pretty x

    ExpErrorLambdaNotAllowedHere x
     ->  text "Lambdas are not allowed here: "  <> pretty x


    ExpErrorValueNotOfType v t
     ->  text "The value " <> pretty v <> text " does not have type " <> pretty t

