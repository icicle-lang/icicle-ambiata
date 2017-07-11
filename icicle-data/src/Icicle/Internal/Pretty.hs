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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Internal.Pretty (
      module PP
    , (<+?>)
    , (<.>)
    , padDoc

    , Pretty(..)
    , Doc
    , Annotation(..)
    , reannotate
    , prettyText
    , parensWhen
    , parensWhenArg
    , prettyArg
    , prettyApp
    , prettyApp'
    , prettySep
    , prettyBinding
    , prettyHeading
    , prettyPunctuation
    , prettyKeyword
    , prettyConstructor
    , prettyStruct
    , prettyStructType
    , prettyH1
    , prettyH2

    , prettyTypedFlat
    , prettyTypedHang
    , prettyTypedBest
    , prettyTypedBest'

    , PrettyFunType(..)
    , prettyTypedFun

    , PrettyItem(..)
    , prettyItems

    , appPrec
    , appPrec1
    ) where

import           Data.String (String)
import           Data.List (replicate, lines, maximum, zipWith)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import           GHC.Show (appPrec, appPrec1)

import           P

import           Text.PrettyPrint.Annotated.WL as PP hiding (Doc, Pretty(..), (<>))
import qualified Text.PrettyPrint.Annotated.WL as WL


type Doc = WL.Doc Annotation

data Annotation =
    AnnError
  | AnnErrorHeading
  | AnnHeading
  | AnnPunctuation
  | AnnKeyword
  | AnnConstant
  | AnnPrimitive
  | AnnBinding
  | AnnVariable
  | AnnConstructor
    deriving (Eq, Ord, Show)

prettyText :: Text -> Doc
prettyText = text . T.unpack

class Pretty a where
  pretty :: a -> Doc
  pretty x =
    prettyPrec 0 x

  prettyPrec :: Int -> a -> Doc
  prettyPrec _ x =
    pretty x

  prettyList :: [a] -> Doc
  prettyList =
    align . prettySep vsep (prettyPunctuation "[") (prettyPunctuation "]") . fmap pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList

instance Pretty Doc where
  pretty        = id

instance Pretty () where
  pretty () =
    prettyPunctuation "()"

instance Pretty Bool where
  pretty b      = WL.pretty b

instance Pretty Char where
  pretty c      = WL.pretty c
  prettyList s  = WL.pretty s

instance Pretty Int where
  pretty i      = WL.pretty i

instance Pretty Int64 where
  pretty i      = WL.pretty i

instance Pretty Integer where
  pretty i      = WL.pretty i

instance Pretty Float where
  pretty f      = WL.pretty f

instance Pretty Double where
  pretty d      = WL.pretty d

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x,y) =
    prettyPunctuation "(" <>
    pretty x <>
    prettyPunctuation "," <+>
    pretty y <>
    prettyPunctuation ")"

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z) =
    prettyPunctuation "(" <>
    pretty x <>
    prettyPunctuation "," <+>
    pretty y <>
    prettyPunctuation "," <+>
    pretty z <>
    prettyPunctuation ")"

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = mempty
  pretty (Just x)       = pretty x

instance Pretty T.Text where
  pretty t = text (T.unpack t)

instance (Pretty l, Pretty r) => Pretty (Either l r) where
  pretty (Left  l) = prettyConstructor "Left"  <+> pretty l
  pretty (Right r) = prettyConstructor "Right" <+> pretty r


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

prettyHeading :: String -> Doc
prettyHeading =
  annotate AnnHeading . text

prettyKeyword :: String -> Doc
prettyKeyword =
  annotate AnnKeyword . text

prettyConstructor :: String -> Doc
prettyConstructor =
  annotate AnnConstructor . text

prettyPunctuation :: String -> Doc
prettyPunctuation =
  annotate AnnPunctuation . text

parensWhen :: Bool -> Doc -> Doc
parensWhen b doc =
  if b then
    prettyPunctuation "(" <> doc <> prettyPunctuation ")"
  else
    doc

parensWhenArg :: Int -> Doc -> Doc
parensWhenArg p =
  parensWhen (p > appPrec)

prettyArg :: Pretty a => a -> Doc
prettyArg =
  prettyPrec appPrec1

prettyApp' :: ([Doc] -> Doc) -> Int -> Doc -> [Doc] -> Doc
prettyApp' xsep p fun args =
  hang 2 . parensWhenArg p . xsep $
    fun : args

prettyApp :: (Pretty a, Pretty b) => ([Doc] -> Doc) -> Int -> a -> [b] -> Doc
prettyApp xsep p fun args =
  prettyApp' xsep p (prettyArg fun) (fmap prettyArg args)

prettySep :: ([Doc] -> Doc) -> Doc -> Doc -> [Doc] -> Doc
prettySep xsep bra ket = \case
  [] ->
    bra <> ket
  [x] ->
    bra <+> x <+> ket
  x : xs ->
    xsep $
      [bra <+> x] <>
      fmap ("," <+>) xs <>
      [prettySepGap xsep <> ket]

prettyTypedFlat :: Doc -> Doc -> Doc
prettyTypedFlat name typ =
  name <+> prettyPunctuation ":" <+> align typ

prettyTypedHang :: Doc -> Doc -> Doc
prettyTypedHang name typ =
  vsep [
      name <+> prettyPunctuation ":"
    , indent 2 $ align typ
    ]

prettyTypedBest' :: Doc -> Doc -> Doc -> Doc
prettyTypedBest' name typ1 typH =
  Union
    (prettyTypedFlat name typ1)
    (prettyTypedHang name typH)

prettyTypedBest :: Doc -> Doc -> Doc
prettyTypedBest name typ =
  prettyTypedBest' name typ typ

data PrettyFunType =
  PrettyFunType {
      typeConstraints :: [Doc]
    , typeArguments :: [Doc]
    , typeResult :: Doc
    }

instance Pretty PrettyFunType where
  pretty typ =
    hsep $ prettyTypedFunParts mempty typ

separators :: Doc -> PrettyFunType -> [Doc]
separators hd (PrettyFunType cs as _) =
  [hd] <>
  replicate (length cs) "=> " <>
  replicate (length as) "-> "

prettyTypedFunParts :: Doc -> PrettyFunType -> [Doc]
prettyTypedFunParts hd pt@(PrettyFunType cs as r) =
  zipWith (<>) (separators hd pt) (cs <> as <> [r])

prettyTypedFun :: Doc -> PrettyFunType -> Doc
prettyTypedFun name0 typ =
  let
    name =
      name0 <+> prettyPunctuation ":"
  in
    Union
      (hsep $ name : prettyTypedFunParts mempty typ)
      (vsep $ name : fmap (indent 2) (prettyTypedFunParts "   " typ))

prettyBinding :: Doc -> Doc -> Doc
prettyBinding n x =
  vsep [
      annotate AnnBinding n <+> prettyPunctuation "="
    , indent 2 x
    ]

prettySepGap :: ([Doc] -> Doc) -> Doc
prettySepGap xsep =
  case lines . show $ xsep ["1", "2"] of
    _ : _ : _ ->
      mempty
    _ ->
      " "

prettyStruct :: (Doc -> Doc -> Doc) -> ([Doc] -> Doc) -> [(Doc, Doc)] -> Doc
prettyStruct xitem xsep = \case
  [] ->
    prettyPunctuation "{}"

  x0 : xs0 ->
    let
      ppField (n, v) =
        xitem (annotate AnnBinding n) v

      x =
        prettyPunctuation "{" <+> ppField x0

      xs =
        fmap ((prettyPunctuation "," <+>) . ppField) xs0
    in
      xsep $
        [x] <> xs <> [prettySepGap xsep <> prettyPunctuation "}"]

prettyStructType :: ([Doc] -> Doc) -> [(Doc, Doc)] -> Doc
prettyStructType =
  prettyStruct prettyTypedFlat

reannotate :: Annotation -> Doc -> Doc
reannotate ann =
  annotate ann . noAnnotate

prettyH1 :: String -> Doc
prettyH1 x =
  vsep [
      prettyHeading x
    , prettyHeading $ replicate (length x) '-'
    ]

prettyH2 :: String -> Doc
prettyH2 x =
  hsep [
      prettyHeading "##"
    , prettyHeading x
    ]

data PrettyItem =
  PrettyItem {
      itemSep :: Doc
    , itemVal :: Doc
    }

catItems :: Doc -> [PrettyItem] -> NonEmpty Doc
catItems end = \case
  [] ->
    end :| []
  PrettyItem sp val : items ->
    let
      x :| xs =
        catItems end items
    in
      val :| sp <+> x : xs

prettyItems :: ([Doc] -> Doc) -> Doc -> [PrettyItem] -> Doc
prettyItems xsep end items =
  let
    hd :| tl =
      catItems end items

    n =
      1 + maximum (0 : fmap (length . show . itemSep) items)
  in
    nest (-n) . xsep $ hd : tl
