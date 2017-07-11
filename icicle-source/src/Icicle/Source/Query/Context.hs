-- | Contexts that filter, group, and do stuff on the input
-- before they hit the expression.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Context (
    Context'  (..)
  , Fold      (..)
  , FoldType  (..)
  , annotOfContext
  ) where

import                  Icicle.Source.Query.Exp
import                  Icicle.Internal.Pretty
import                  Icicle.Common.Base

import                  P


data Context' q a n
 = Windowed a WindowUnit (Maybe WindowUnit)
 | Latest a Int
 | GroupBy   a          (Exp' q a n)
 | Distinct  a          (Exp' q a n)
 | Filter    a          (Exp' q a n)
 | LetFold   a          (Fold q a n)
 | Let       a (Name n) (Exp' q a n)
 | GroupFold a (Name n) (Name n) (Exp' q a n)
 deriving (Show, Eq, Ord)

data Fold q a n
 = Fold
 { foldBind :: Name n
 , foldInit :: Exp' q a n
 , foldWork :: Exp' q a n
 , foldType :: FoldType }
 deriving (Show, Eq, Ord)

data FoldType
 = FoldTypeFoldl1
 | FoldTypeFoldl
 deriving (Show, Eq, Ord)


annotOfContext :: Context' q a n -> a
annotOfContext c
 = case c of
    Windowed  a _ _   -> a
    Latest    a _     -> a
    GroupBy   a _     -> a
    GroupFold a _ _ _ -> a
    Distinct  a _     -> a
    Filter    a _     -> a
    LetFold   a _     -> a
    Let       a _ _   -> a

instance (Pretty n, Pretty q) => Pretty (Context' q a n) where
  pretty = \case
    Windowed _ newer Nothing ->
      prettyKeyword "windowed" <+> pretty newer

    Windowed _ newer (Just older) ->
      prettyKeyword "windowed between" <+>
      pretty older <+>
      prettyKeyword "and" <+>
      pretty newer

    Latest _ i ->
      prettyKeyword "latest" <+> annotate AnnConstant (pretty i)

    GroupBy _ x ->
      prettyKeyword "group" <+> align (pretty x)

    GroupFold _ n1 n2 x ->
      vsep [
          prettyKeyword "group fold" <+> pretty (n1, n2) <+> prettyPunctuation "="
        , indent 2 . align $
            pretty x
        ]

    Distinct _ x ->
      prettyKeyword "distinct" <+> align (pretty x)

    Filter _ x ->
      prettyKeyword "filter" <+> align (pretty x)

    LetFold _ f ->
      vsep [
          pretty (foldType f) <+> annotate AnnBinding (pretty (foldBind f)) <+> prettyPunctuation "="
        , indent 2 . align $
            pretty (foldInit f) <+> prettyPunctuation ":" <+> pretty (foldWork f)
        ]

    Let _ b x ->
      vsep [
          prettyKeyword "let" <+> annotate AnnBinding (pretty b) <+> prettyPunctuation "="
        , indent 2 . align $
            pretty x
        ]

instance Pretty FoldType where
  pretty = \case
    FoldTypeFoldl1 ->
      prettyKeyword "fold1"
    FoldTypeFoldl ->
      prettyKeyword "fold"
