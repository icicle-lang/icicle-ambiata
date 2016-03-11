-- | Provide a *slightly* better interface to pretty printer.
--
-- Problem is that it defines (<>), but so does Data.Monoid.
--
-- Its (<>) really is a monoid though, so hiding it and adding
-- an orphan instance that does the same thing seems fine to me.
--
-- The pretty printer also defines <$> which isn't an applicative.
-- We will just hide that one.
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Internal.Pretty (
    module PP
    , (<+?>)
    , (<.>)
    , padDoc
    , Pretty (..)
    , Doc
    , Annotation (..)
    , annSepByComma
    , annTypeArgs
    , annotateTypeArgs
    ) where

-- The one we want to export without <> or <$>
import              Text.PrettyPrint.Annotated.Leijen as PP hiding ((<>), (<$>), Doc)
-- The one with <>
import qualified    Text.PrettyPrint.Annotated.Leijen as PJOIN

import              P

import              Data.List (replicate, lines)
import qualified    Data.Text               as T

type Doc = PJOIN.Doc Annotation

data Annotation
 = AnnVariable
 -- Type annotations.
 -- Why aren't these specific? This module is first to import, and all types export Pretty.
 | forall a. Pretty a => AnnType a

annSepByComma :: [Doc] -> Annotation
annSepByComma = AnnType . go . fmap pretty
  where
    go []     = ""
    go [x]    = x
    go (x:xs) = x <.> go xs

-- | Annotate with the type of arguments.
--
annTypeArgs :: Pretty a => [a] -> Annotation
annTypeArgs = annSepByComma . fmap pretty

annotateTypeArgs :: Pretty a => [a] -> Doc -> Doc
annotateTypeArgs ts = annotate (annTypeArgs ts)

instance Monoid Doc where
 mempty  =  PJOIN.empty
 mappend = (PJOIN.<>)

class Pretty a where
  pretty        :: a -> Doc
  prettyList    :: [a] -> Doc
  prettyList    = list . fmap pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList

instance Pretty Doc where
  pretty        = id

instance Pretty () where
  pretty ()     = PP.text "()"

instance Pretty Bool where
  pretty b      = PP.bool b

instance Pretty Char where
  pretty c      = PP.char c
  prettyList s  = PP.string s

instance Pretty Int where
  pretty i      = PP.int i

instance Pretty Integer where
  pretty i      = PP.integer i

instance Pretty Float where
  pretty f      = PP.float f

instance Pretty Double where
  pretty d      = PP.double d

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = PP.empty
  pretty (Just x)       = pretty x

instance Pretty T.Text where
  pretty t = text (T.unpack t)

-- | Concatenate two possibly-empty documents, separated by spaces.
-- This probably shouldn't be used for large documents.
(<+?>) :: Doc -> Doc -> Doc
a <+?> b
 | null $ show a
 = b
 | null $ show b
 = a
 | otherwise
 = a <+> b

(<.>) :: Doc -> Doc -> Doc
a <.> b
 = pretty a <> "," <+> pretty b

-- | Pad a document out to some width
-- left aligned
padDoc :: Int -> Doc -> Doc
padDoc wid doc
 = let d' = lines $ show doc
       w  = maybe 0 length $ lastMaybe d'
   in  if w <= wid
       then doc P.<> mconcat (replicate (wid - w) (text " "))
       else doc P.<> line P.<> mconcat (replicate wid (text " "))
