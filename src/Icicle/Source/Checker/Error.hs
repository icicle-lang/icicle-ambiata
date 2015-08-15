{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Error (
    CheckError(..)
  , ErrorInfo(..)
  , ErrorSuggestion(..)
  , annotOfError
  , errorNoSuggestions
  , errorSuggestions
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import                  Icicle.Internal.Pretty

import                  P

import                  Data.String

data CheckError a n
 = CheckError (ErrorInfo a n) [ErrorSuggestion a n]
 deriving (Show, Eq, Ord)

data ErrorInfo a n
 = ErrorNoSuchVariable a (Name n)
 | ErrorNoSuchFeature (Name n)
 | ErrorContextNotAllowedHere  a (Context a n)
 | ErrorFunctionWrongArgs      a (Exp a n) (FunctionType n) [Type n]
 | ErrorApplicationNotFunction a (Exp a n)
 | ErrorConstraintsNotSatisfied a [(a, DischargeError n)]
 | ErrorReturnNotAggregate a (Type n)
 deriving (Show, Eq, Ord)

annotOfError :: CheckError a n -> Maybe a
annotOfError (CheckError e _)
 = case e of
    ErrorNoSuchVariable a _
     -> Just a
    ErrorNoSuchFeature _
     -> Nothing
    ErrorContextNotAllowedHere  a _
     -> Just a
    ErrorFunctionWrongArgs      a _ _ _
     -> Just a
    ErrorApplicationNotFunction a _
     -> Just a
    ErrorConstraintsNotSatisfied          a _
     -> Just a
    ErrorReturnNotAggregate          a _
     -> Just a


data ErrorSuggestion a n
 = AvailableFeatures [(Name n, Type n)]
 | AvailableBindings [(Name n, FunctionType n)]
 | Suggest String
 deriving (Show, Eq, Ord)


-- Helpers ------------
errorNoSuggestions :: ErrorInfo a n -> Either (CheckError a n) r
errorNoSuggestions info
 = Left $ CheckError info []

errorSuggestions :: ErrorInfo a n -> [ErrorSuggestion a n] -> Either (CheckError a n) r
errorSuggestions info sugs
 = Left $ CheckError info sugs

-- Pretties ----------

instance (Pretty a, Pretty n) => Pretty (CheckError a n) where
 pretty (CheckError info [])
  = pretty info
 pretty (CheckError info sugs)
  = pretty info <> line
  <> "Extra information:" <> line
  <> indent 2 (vcat $ fmap pretty sugs)

instance (Pretty a, Pretty n) => Pretty (ErrorInfo a n) where
 pretty e
  = case e of
     ErrorNoSuchVariable a n
      -> "Unknown variable" <+> pretty n <+> "at" <+> pretty a
     ErrorNoSuchFeature n
      -> "The dictionary has no feature called" <+> pretty n

     ErrorContextNotAllowedHere  a c
      -> "Context is not allowed at" <+> pretty a <> line
      <> "Context: " <> inp c

     ErrorFunctionWrongArgs a x f tys
      -> "Function applied to wrong number of arguments at " <+> pretty a <> line
      <> "Expression:     " <> inp x
      <> "Function type:  " <> inp f
      <> "Argument types: " <> inp tys

     ErrorApplicationNotFunction a x
      -> "Application of non-function at " <+> pretty a <> line
      <> "Exp: " <> inp x

     ErrorConstraintsNotSatisfied a ds
      -> "Cannot discharge constraints at " <+> pretty a <> line
      <> "Constraints: " <> cat (fmap ((<+>" ").pretty) ds)

     ErrorReturnNotAggregate a t
      -> "Return type is not an aggregate at " <+> pretty a <> line
      <> "Type: " <> inp t


   where
    inp x = indent 0 (pretty x)
 

instance (Pretty a, Pretty n) => Pretty (ErrorSuggestion a n) where
 pretty e
  = case e of
     AvailableFeatures bs
      -> "The features in this dictionary are:"
      <> indent 2 (vcat $ fmap pretty_ty bs)

     AvailableBindings bs
      -> "The available bindings are:"
      <> indent 2 (vcat $ fmap pretty_ty bs)


     Suggest str
      -> text str

  where
   pretty_ty (k,t) = pretty k <+> ":" <+> pretty t

