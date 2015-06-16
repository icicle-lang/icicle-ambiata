{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Error (
    CheckError(..)
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Internal.Pretty

import                  P

data CheckError a n
 = ErrorNoSuchVariable a n
 | ErrorNoSuchFeature n
 | ErrorReturnNotAggregate a (Query a n) UniverseType
 | ErrorContextExpNotBool  a (Context a n)   UniverseType
 | ErrorContextExpNotEnum  a (Context a n)   UniverseType
 | ErrorContextExpNotElem  a (Context a n)   UniverseType
 | ErrorFoldTypeMismatch       a UniverseType UniverseType
 | ErrorApplicationOfNonPrim a (Exp a n)
 | ErrorPrimBadArgs          a (Exp a n) [UniverseType]
 deriving (Show, Eq, Ord)


instance (Pretty a, Pretty n) => Pretty (CheckError a n) where
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

     ErrorFoldTypeMismatch a init work
      -> "Type mismatch in fold at " <+> pretty a <> line
      <> "Initial: " <> inp init    <> line
      <> "Worker:  " <> inp work

     ErrorApplicationOfNonPrim a x
      -> "Application of non-function at " <+> pretty a <> line
      <> "Exp: " <> inp x

     ErrorPrimBadArgs a x tys
      -> "Primitive applied to bad arguments at " <+> pretty a <> line
      <> "Exp:  " <> inp x <> line
      <> "Args: " <> cat (fmap ((<+>" ").pretty) tys)


   where
    inp x = indent 0 (pretty x)
 

