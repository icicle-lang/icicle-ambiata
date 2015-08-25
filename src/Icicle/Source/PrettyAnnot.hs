{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Icicle.Source.PrettyAnnot (
    PrettyAnnot(..)
  ) where

import                  Icicle.Source.Query
import                  Icicle.Internal.Pretty

import                  P

import                  Data.List (intersperse)

newtype PrettyAnnot q
 = PrettyAnnot
 { getPrettyAnnot :: q }

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Exp a n)) where
 pretty xx
  = case getPrettyAnnot xx of
     Var a n    -> pretty n               <> "@{" <> pretty a <> "}"
     Nested a q -> line <> indent 2 ("(" <> pretty (PrettyAnnot q) <> ")@{" <> pretty a <> "}")
     App{}
      -> let (f,xs) = takeApps $ getPrettyAnnot xx
         in  "(" <> pretty (PrettyAnnot f) <> " " <> hsep (fmap (pretty.PrettyAnnot) xs) <> ")"

     Prim a (Op o)
      -> "(" <> pretty o <> ")@{" <> pretty a <> "}"
     Prim a p
      -> pretty p <> "@{" <> pretty a <> "}"
     Case a scrut pats
      -> indent 0
      (  "case@{" <> pretty a <> "}" <> line
      <> indent 2 (pretty (PrettyAnnot scrut)) <> line
      <> indent 2 (vcat $ fmap (\(p,x) -> " | " <> pretty p <> " -> " <> pretty (PrettyAnnot x)) pats) <> line
      <> "end")


instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Context a n)) where
 pretty cc
  = case getPrettyAnnot cc of
     Windowed a _ _ -> "windowed@{" <> pretty a <> "}"
     Latest a _     -> "latest@{" <> pretty a <> "}" 
     GroupBy a x    -> "group@{" <> pretty a <> "} " <> pretty (PrettyAnnot x)
     Distinct a x    -> "distinct@{" <> pretty a <> "} " <> pretty (PrettyAnnot x)
     Filter   a x    -> "filter@{" <> pretty a <> "} " <> pretty (PrettyAnnot x)
     LetFold  a f
      ->  "let@{" <> pretty a <> "}"
      <+> pretty (foldType f) <> pretty (foldBind f) <> line
      <> indent 2 ("=" <+> pretty (PrettyAnnot $ foldInit f) <> line
                <> ":" <+> pretty (PrettyAnnot $ foldWork f))

     Let    a n x    -> "let@{" <> pretty a <> "} " <> pretty n <> line
                     <> indent 2 (" = " <> pretty (PrettyAnnot x))


instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Query a n)) where
 pretty qq
  = case getPrettyAnnot qq of
     Query cs x
      -> mconcat
       $ intersperse
            (line <> " ~> ")
            ( fmap (indent 0 . pretty . PrettyAnnot) cs
            <> [pretty $ PrettyAnnot x])

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (QueryTop a n)) where
 pretty qq
  = case getPrettyAnnot qq of
     QueryTop f q -> "feature " <> pretty f <> line <> " ~> " <> pretty (PrettyAnnot q)


