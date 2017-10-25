{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Canyon.Statement (
    Statement       (..)
  , Avalanche.FactBinds (..)
  , Avalanche.factBindsAll
  , Avalanche.WhileType (..)
  , Avalanche.ForeachType (..)
  ) where

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Exp

import           Icicle.Data.Name

import           Icicle.Internal.Pretty
import qualified Icicle.Avalanche.Statement.Statement as Avalanche
import           Icicle.Avalanche.Prim.Flat

import           P


-- | Part of a loop
data Statement a n
 = Let !(Name n) !ValType !(Exp a n Prim) !(Statement a n)
 | Write  !(Name n) !(Exp a n Prim)

 | If           !(Exp a n Prim) !(Statement a n) !(Statement a n)
 | While        !Avalanche.WhileType   !(Name n) !ValType !(Exp a n Prim) !(Statement a n)
 | ForeachInts  !Avalanche.ForeachType !(Name n) !(Exp a n Prim) !(Exp a n Prim) !(Statement a n)
 | ForeachFacts !(Avalanche.FactBinds n) !ValType !(Statement a n)
 | Block ![Statement a n]
 | Output !OutputId !ValType ![(Exp a n Prim, ValType)]
 | LoadResumable !(Name n) !ValType
 | SaveResumable !(Name n) !ValType
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (Statement a n)

instance Monoid (Statement a n) where
 mempty = Block []
 mappend p q
        = Block [p, q]

-- Pretty printing -------------

flattenBlocks :: Statement a n -> [Statement a n]
flattenBlocks = \case
  Block xs ->
    concatMap flattenBlocks xs
  x ->
    [x]

instance (Pretty n) => Pretty (Statement a n) where
  pretty = \case
    If x stmts elses ->
      line <>
      prettyKeyword "if" <+> prettyPunctuation "(" <> pretty x <> prettyPunctuation ")" <> line <>
      subscope stmts <>
      case elses of
        Block [] ->
          line
        _ ->
          line <>
          prettyKeyword "else" <> line <>
          subscope elses <>
          line

    Let n t e stmts ->
      annotate AnnBinding (pretty n) <+> ":" <+> pretty t <+> "=" <+> pretty e <> line <>
      nosubscope stmts

    Write n x ->
      annotate AnnBinding (pretty n) <+> prettyPunctuation "=" <+> pretty x

    While t n _ end stmts ->
      line <>
      prettyKeyword "while" <+> prettyPunctuation "(" <> annotate AnnVariable (pretty n) <+> pretty t <+> pretty end <> ")" <> line <>
      subscope stmts <>
      line

    ForeachInts _ n from to stmts ->
      line <>
      prettyKeyword "foreach" <+>
        prettyPunctuation "(" <> annotate AnnBinding (pretty n) <+>
        prettyKeyword "in" <+> pretty from <+>
        prettyPunctuation ".." <+> pretty to <>
        prettyPunctuation ")" <> line <>
      subscope stmts <>
      line

    ForeachFacts binds _ stmts ->
      line <>
      prettyKeyword "for_facts" <+>
        prettyFactParts AnnBinding (Avalanche.factBindsAll binds) <+>
      subscope stmts <>
      line

    x@(Block _) ->
     -- We don't actually need to indent here,
     -- because it doesn't really introduce scope
     vcat (fmap pretty $ flattenBlocks x)

    Output n t xs ->
      line <>
      prettyKeyword "output" <+>
        prettyTypedFlat (annotate AnnBinding $ pretty n) (pretty t) <+> prettyPunctuation "=" <> line <>
      case xs of
        [x] ->
          -- output =
          --   foo
          indent 2 $ prettyFactPart AnnVariable x
        _ ->
          -- output =
          --     foo
          --   , bar
          indent 4 $ prettyFactParts AnnVariable xs

    LoadResumable n _t ->
      prettyKeyword "load_resumable" <+> annotate AnnVariable (pretty n)

    SaveResumable n _t ->
      prettyKeyword "save_resumable" <+> annotate AnnVariable (pretty n)

   where
    subscope stmt
     = vcat
     [ prettyPunctuation "{"
     , indent 4 (pretty stmt)
     , prettyPunctuation "}"]

    -- We don't want to indent for every let or read just for aesthetic reasons:
    -- it gets messy very quickly
    nosubscope stmt
     = pretty stmt

    prettyFactPart ann (nf, tf) =
      prettyTypedFlat (annotate ann $ pretty nf) (pretty tf)

    prettyFactParts ann xs0 =
      case reverse xs0 of
        [] ->
          mempty
        x : xs1 ->
          align . prettyItems vsep (prettyFactPart ann x) $
            fmap (PrettyItem "," . prettyFactPart ann) (reverse xs1)

