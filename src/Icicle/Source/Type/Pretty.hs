-- | Pretty for functions
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Pretty (
    prettyFunWithNames
  , prettyFunWithLetters
  , prettyFunFromStrings
  , letterNames
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Subst
import                  Icicle.Source.Lexer.Token

import                  Icicle.Internal.Pretty

import                  P

import                  Data.String
import                  Data.List (zip)
import qualified        Data.Map as Map
import qualified        Data.Text as T


-- | This is a rather dodgy trick.
-- Function types are generalised with fresh variable names,
-- however fresh variable names are quite ugly.
-- Instead of actually using nice names, we will clean them up
-- just before pretty printing.
prettyFunWithNames :: (Ord n, Pretty n) => [Name n] -> FunctionType n -> Doc
prettyFunWithNames names fun
  =  constrs (functionConstraints   fun')
  <> args    (functionArguments     fun')
  <> pretty  (functionReturn        fun')
  where
   sub
    = Map.fromList
    (functionForalls fun `zip` fmap TypeVar names)

   fun' = substFT sub (fun { functionForalls = [] })

   constrs []
    = ""
   constrs xs
    = tupled (fmap pretty xs) <> " => "

   args xs
    = hsep (fmap (\x -> pretty x <+> "-> ") xs)

prettyFunFromStrings :: (IsString n, Pretty n, Ord n) => FunctionType n -> Doc
prettyFunFromStrings
 = prettyFunWithNames
 $ fmap (Name . fromString) letterNames

prettyFunWithLetters :: FunctionType Variable -> Doc
prettyFunWithLetters
 = prettyFunWithNames
 $ fmap (Name . Variable . T.pack) letterNames

letterNames :: [String]
letterNames
 =  fmap (\c -> [c]) ['a'..'z']
 <> concatMap (\prefix -> fmap (\c -> prefix <> [c]) ['a'..'z']) letterNames


