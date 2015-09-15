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
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Internal.Pretty (
    module PP
    , (<+?>)
    , padDoc
    ) where

-- The one we want to export without <> or <$>
import              Text.PrettyPrint.Leijen as PP hiding ((<>), (<$>))
-- The one with <>
import              Text.PrettyPrint.Leijen as PJOIN

import              P

import              Data.List (replicate, lines)
import qualified    Data.Text               as T
import              Data.String (IsString(..))

instance Monoid Doc where
 mempty  =  PJOIN.empty
 mappend = (PJOIN.<>)

-- We also need to be able to pretty Data.Text...
instance Pretty T.Text where
 pretty t = text (T.unpack t)

-- String literals are nice to have.
instance IsString Doc where
 fromString = text

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


-- | Pad a document out to some width
-- left aligned
padDoc :: Int -> Doc -> Doc
padDoc wid doc
 = let d' = lines $ show doc
       w  = maybe 0 length $ lastMaybe d'
   in  if w <= wid
       then doc P.<> mconcat (replicate (wid - w) (text " "))
       else doc P.<> line P.<> mconcat (replicate wid (text " "))
