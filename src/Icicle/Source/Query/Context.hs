-- | Contexts that filter, group, and do stuff on the input
-- before they hit the expression.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Context (
    Context'  (..)
  , WindowUnit(..)
  , Fold      (..)
  , FoldType  (..)
  , Sort      (..)
  ) where

import                  Icicle.Source.Query.Exp
import                  Icicle.Internal.Pretty

import                  P


data Context' q n
 = Windowed WindowUnit (Maybe WindowUnit)
 | Latest Int
 | GroupBy              (Exp' q n)
 | Distinct             (Exp' q n)
 | Filter               (Exp' q n)
 | LetFold              (Fold q n)
 | Let (Maybe Sort) n   (Exp' q n)
 deriving (Show, Eq, Ord)

data WindowUnit
 = Days   Int
 | Months Int
 | Weeks  Int
 deriving (Show, Eq, Ord)

data Fold q n
 = Fold
 { foldBind :: n
 , foldInit :: Exp' q n
 , foldWork :: Exp' q n
 , foldType :: FoldType }
 deriving (Show, Eq, Ord)

data FoldType
 = FoldTypeFoldl1
 -- | FoldTypeFoldl
 deriving (Show, Eq, Ord)

data Sort = Stream | Aggregate
 deriving (Show, Eq, Ord)


instance (Pretty n, Pretty q) => Pretty (Context' q n) where
 pretty cc
  = case cc of
     Windowed newer Nothing
      -> "windowed" <+> pretty newer
     Windowed newer (Just older)
      -> "windowed between" <+> pretty older
                  <+> "and" <+> pretty newer

     Latest   i
      -> "latest"   <+> pretty i
     GroupBy  x
      -> "group"    <+> pretty x
     Distinct x
      -> "distinct" <+> pretty x
     Filter   x
      -> "filter"   <+> pretty x
     LetFold f
      ->  "let fold"
      <+> pretty (foldBind f)
      <+> "="
      <+> pretty (foldInit f)
      <+> ":"
      <+> pretty (foldWork f)

     Let _ b x
      ->  "let"
      <+> pretty b
      <+> "="
      <+> pretty x

     
instance Pretty WindowUnit where
 pretty wu
  = case wu of
     Days   i -> pretty i <+> "days"
     Months i -> pretty i <+> "months"
     Weeks  i -> pretty i <+> "weeks"

