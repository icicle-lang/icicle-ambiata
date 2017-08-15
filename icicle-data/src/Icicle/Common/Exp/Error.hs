-- | Errors that can occur when typechecking an expression
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Error (
      ExpError (..)
    , ExpErrorContext (..)
    ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Exp.Exp

import           P


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

 -- In the let binding "foo"...
 | ExpErrorInContext !(ExpErrorContext n) !(ExpError a n p)
 deriving (Show, Eq, Ord, Generic)

data ExpErrorContext n
 = ExpErrorContextLet !(Name n)
 | ExpErrorContextLambda !(Name n) !ValType
 deriving (Show, Eq, Ord, Generic)

instance (NFData a, NFData n, NFData p) => NFData (ExpError a n p)
instance (NFData n) => NFData (ExpErrorContext n)

instance (Pretty n, Pretty p) => Pretty (ExpError a n p) where
 pretty e
  = case e of
    ExpErrorVarNotInEnv n
     -> text "Variable not bound: " <> pretty n

    ExpErrorApp fun arg (FunT (funt:_) _) argt
     ->  text "Application type mismatch:"
     <> line
     <>  indent 2 ( text "Fun: " <> pretty fun              <> line
                 <> text "Arg: " <> pretty arg              <> line
                 <> text "Expected: " <> pretty funt        <> line
                 <> text "Actual:   " <> pretty argt        <> line)
    ExpErrorApp fun arg (FunT [] funt) argt
     ->  text "Application of non-function:"
     <> line
     <>  indent 2 ( text "Fun:  " <> pretty fun             <> line
                 <> text "Type: " <> pretty funt            <> line
                 <> text "Arg:  " <> pretty arg             <> line
                 <> text "Type: " <> pretty argt            <> line)

    ExpErrorNameNotUnique n
     ->  text "Bound name is not unique: " <> pretty n                  <> line
     <>  text "(for simplicity, we require all core names to be unique)"

    ExpErrorPrimitiveNotFullyApplied p x
     ->  text "The primitive " <> pretty p <> text " is not fully applied in expression " <> pretty x

    ExpErrorLambdaNotAllowedHere x
     ->  text "Lambdas are not allowed here: "  <> pretty x


    ExpErrorValueNotOfType v t
     ->  text "The value " <> pretty v <> text " does not have type " <> pretty t

    ExpErrorInContext (ExpErrorContextLet n) e'
     -> text "In let-binding " <> pretty n <> line <>
        pretty e'

    ExpErrorInContext (ExpErrorContextLambda n t) e'
     -> text "In lambda-binding " <> pretty n <> text " : " <> pretty t <> line <>
        pretty e'
