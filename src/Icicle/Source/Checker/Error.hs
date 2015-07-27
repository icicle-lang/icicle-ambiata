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

import                  Icicle.Internal.Pretty

import                  P

import                  Data.String

data CheckError a n
 = CheckError (ErrorInfo a n) [ErrorSuggestion a n]
 deriving (Show, Eq, Ord)

data ErrorInfo a n
 = ErrorNoSuchVariable a n
 | ErrorNoSuchFeature n
 | ErrorReturnNotAggregate a (Query a n) UniverseType
 | ErrorContextExpNotBool  a (Context a n)   UniverseType
 | ErrorContextExpNotEnum  a (Context a n)   UniverseType
 | ErrorContextExpNotElem  a (Context a n)   UniverseType
 | ErrorContextNotAllowedHere  a (Context a n)
 | ErrorFoldTypeMismatch       a UniverseType UniverseType
 | ErrorLetTypeMismatch        a UniverseType UniverseType
 | ErrorUniverseMismatch       a UniverseType Universe
 | ErrorApplicationOfNonPrim a (Exp a n)
 | ErrorPrimBadArgs          a (Exp a n) [UniverseType]
 | ErrorPrimNotANumber       a (Exp a n) [UniverseType]
 deriving (Show, Eq, Ord)

annotOfError :: CheckError a n -> Maybe a
annotOfError (CheckError e _)
 = case e of
    ErrorNoSuchVariable a _
     -> Just a
    ErrorNoSuchFeature _
     -> Nothing
    ErrorReturnNotAggregate a _ _
     -> Just a
    ErrorContextExpNotBool  a _ _
     -> Just a
    ErrorContextExpNotEnum  a _ _
     -> Just a
    ErrorContextExpNotElem  a _ _
     -> Just a
    ErrorContextNotAllowedHere  a _
     -> Just a
    ErrorFoldTypeMismatch       a _ _
     -> Just a
    ErrorLetTypeMismatch        a _ _
     -> Just a
    ErrorUniverseMismatch       a _ _
     -> Just a
    ErrorApplicationOfNonPrim a _
     -> Just a
    ErrorPrimBadArgs          a _ _
     -> Just a
    ErrorPrimNotANumber       a _ _
     -> Just a


data ErrorSuggestion a n
 = AvailableFeatures [(n, BaseType)]
 | AvailableBindings [(n, UniverseType)]
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

     ErrorReturnNotAggregate a q ut
      -> "The return of the query is not an aggregate at" <+> pretty a <> line
      <> "Query: " <> inp q <> line
      <> "Type:  " <> inp ut

     ErrorContextExpNotBool a c ut
      -> "Context expression is not a bool at" <+> pretty a <> line
      <> "Context: " <> inp c       <> line
      <> "Type:    " <> inp ut

     ErrorContextExpNotEnum a c ut
      -> "Context expression is not enum (int, date,..) at" <+> pretty a <> line
      <> "Context: " <> inp c       <> line
      <> "Type:    " <> inp ut

     ErrorContextExpNotElem a c ut
      -> "Context expression is not an element expression (cannot be aggregate) at" <+> pretty a <> line
      <> "Context: " <> inp c       <> line
      <> "Type:    " <> inp ut

     ErrorContextNotAllowedHere  a c
      -> "Context is not allowed at" <+> pretty a <> line
      <> "Context: " <> inp c

     ErrorFoldTypeMismatch a init work
      -> "Type mismatch in fold at " <+> pretty a <> line
      <> "Initial: " <> inp init    <> line
      <> "Worker:  " <> inp work

     ErrorLetTypeMismatch  a ty expected
      -> "Type mismatch in let at " <+> pretty a <> line
      <> "Type:     " <> inp ty    <> line
      <> "Expected: " <> inp expected

     ErrorUniverseMismatch a ty expected
      -> "Universe mismatch at " <+> pretty a <> line
      <> "Type:     " <> inp ty      <> line
      <> "Expected: " <> text (show expected)

     ErrorApplicationOfNonPrim a x
      -> "Application of non-function at " <+> pretty a <> line
      <> "Exp: " <> inp x

     ErrorPrimBadArgs a x tys
      -> "Primitive applied to bad arguments at " <+> pretty a <> line
      <> "Exp:  " <> inp x <> line
      <> "Args: " <> cat (fmap ((<+>" ").pretty) tys)

     ErrorPrimNotANumber a x tys
      -> "This operater requires a number type at " <+> pretty a <> line
      <> "Exp:  " <> inp x <> line
      <> "Args: " <> cat (fmap ((<+>" ").pretty) tys)


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

