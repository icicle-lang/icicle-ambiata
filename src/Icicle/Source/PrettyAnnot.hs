{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Source.PrettyAnnot (
    PrettyAnnot(..)
  ) where

import                  Icicle.Source.Type
import                  Icicle.Source.Query
import                  Icicle.Internal.Pretty

import                  P

newtype PrettyAnnot q
 = PrettyAnnot
 { getPrettyAnnot :: q }

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Exp (Annot a n) n)) where
 pretty xx
  = prettyX 0 (getPrettyAnnot xx)

prettyX :: (Pretty a, Pretty n, Pretty q, Pretty (PrettyAnnot q)) => Int -> Exp' q (Annot a n) n -> Doc
prettyX outer_prec xx
 = wrap
 $ case xx of
    App{}
     -- Operators
     | Just (Op o, a, [x]) <- takePrimApps xx
     , FPrefix <- fixity o
     -> annotate (AnnType a) (pretty o) <+> prettyX inner_prec x
     | Just (Op o, a, [x,y]) <- takePrimApps xx
     , FInfix _ <- fixity o
     ->  prettyX inner_prec_1 x
     </> annotate (AnnType a) (pretty o)
     <+> prettyX inner_prec_2 y

    App _ x y
     ->  prettyX inner_prec_1 x
     <+> prettyX inner_prec_2 y

    Var a n
     -> annotate (AnnType a) $ pretty n

    Prim a p
     -> annotate (AnnType a) $ pretty p

    Nested a q
     -> annotate (AnnType a) $ pretty (PrettyAnnot q)

    Case a scrut pats
     -> indent 0 $ annotate (AnnType a)
     (  "case" <+> prettyX 0 scrut <> line
     <> indent 2 (vcat $ fmap (\(p,x) -> "| " <> pretty p <> " -> " <> prettyX 0 x) pats) <> line
     <> "end")

 where
  (inner_prec, assoc) = precedenceOfX xx

  -- Precedence of first operator argument
  inner_prec_1
   | AssocLeft <- assoc
   = inner_prec
   | otherwise
   = inner_prec + 1

  -- Precedence of second operator argument
  inner_prec_2
   | AssocRight <- assoc
   = inner_prec
   | otherwise
   = inner_prec + 1

  -- Suppose we have
  --
  --   7   6   7   (precedences)
  -- a * b + c * d
  --
  --        +        6
  --      /   \
  --     *     *     7
  --    / \   / \
  --   a   b c   d
  --
  -- So when pretty printing, the precedence is allowed to increase
  -- without requiring parentheses, but decreasing needs them.
  --
  wrap
   | inner_prec < outer_prec
   = parens . indent 0
   | otherwise
   = id


instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Context (Annot a n) n)) where
 pretty cc
  = case getPrettyAnnot cc of
     Windowed  a n Nothing
                       -> annotate (AnnType a) ("windowed"  <+> pretty n)
     Windowed  a n (Just o)
                       -> annotate (AnnType a) ("windowed between" <+> pretty o
                                                           <+> "and" <+> pretty n)
     Latest    a x     -> annotate (AnnType a) ("latest"    <+> pretty x)
     GroupBy   a x     -> annotate (AnnType a)  "group"     <+> pretty (PrettyAnnot x)
     GroupFold a n m x -> annotate (AnnType a) ("group fold"<+> pretty (n, m))
                                                              <+> "="
                                                              <+> pretty (PrettyAnnot x)
     Distinct  a x     -> annotate (AnnType a)  "distinct"  <+> pretty (PrettyAnnot x)
     Filter    a x     -> annotate (AnnType a)  "filter"    <+> pretty (PrettyAnnot x)
     LetFold   a f     -> annotate (AnnType a) ("let"       <+> pretty (foldType f) <+> pretty (foldBind f))
                       <> line
                       <> indent 2 ( "=" <+> pretty (PrettyAnnot $ foldInit f) <> line <>
                                     ":" <+> pretty (PrettyAnnot $ foldWork f))

     Let       a n x   -> annotate (AnnType a) ("let"       <+> pretty n)
                       <> line
                       <> indent 2 (" = " <> pretty (PrettyAnnot x))


instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (Query (Annot a n) n)) where
 pretty (PrettyAnnot q)
  =  cat (fmap (\c -> inp c <> line <> "~> ") (contexts q))
  <> inp                                      (final    q)
  where
  inp p = indent 0 $ pretty $ PrettyAnnot p

instance (Pretty a, Pretty n) => Pretty (PrettyAnnot (QueryTop (Annot a n) n)) where
 pretty qq
  = case getPrettyAnnot qq of
     QueryTop f _ q -> "feature" <+> pretty f <> line <> "~>" <+> pretty (PrettyAnnot q)


