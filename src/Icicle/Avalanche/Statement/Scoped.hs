-- | Statements in a scoped way.
-- Some representations are better for different things:
-- the inner/nested way of statement is better for type checking and evaluating,
-- but tedious for inserting new let bindings.
--
-- This Scoped representation makes inserting bindings better.
--
-- It's also nicer for pretty printing to your average imperative
-- language, since the block structure of an imperative language
-- don't actually correspond to the scopes.
-- 
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Statement.Scoped (
    Scoped          (..)
  , Binding         (..)
  , scopedOfStatement
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import qualified    Icicle.Avalanche.Statement.Statement as S

import              Icicle.Internal.Pretty

import              P


data Scoped n p
 = If   (Exp n p)   (Scoped n p)    (Scoped n p)
 | ForeachInts  (Name n) (Exp n p)  (Exp n p) (Scoped n p)
 | ForeachFacts (Name n) ValType    (Scoped n p)
 | Block                        [Binding n p] [Scoped n p]
 | Write (Name n) (Exp n p)
 | Push  (Name n) (Exp n p)
 | Return         (Exp n p)

data Binding n p
 = InitAccumulator (S.Accumulator n p)
 | Let             (Name n) (Exp n p)
 | Read            (Name n) (Name n)

scopedOfStatement :: S.Statement n p -> Scoped n p
scopedOfStatement s
 = case bindsOfStatement s of
    ([], [s']) -> s'
    (bs, ss) -> Block bs ss

bindsOfStatement :: S.Statement n p -> ([Binding n p], [Scoped n p])
bindsOfStatement s
 = case s of
    S.If x ss es
     -> ([], [If x (scopedOfStatement ss) (scopedOfStatement es)])
    S.ForeachInts n from to ss
     -> ([], [ForeachInts n from to (scopedOfStatement ss)])
    S.ForeachFacts n vt ss
     -> ([], [ForeachFacts n vt (scopedOfStatement ss)])
    S.Block ss
     -> ([], fmap scopedOfStatement ss)
    S.Write n x
     -> ([], [Write n x])
    S.Push n x
     -> ([], [Push n x])
    S.Return x
     -> ([], [Return x])
    S.InitAccumulator acc ss
     -> let (bs,s') = bindsOfStatement ss
        in  (InitAccumulator acc : bs, s')
    S.Let n x ss
     -> let (bs,s') = bindsOfStatement ss
        in  (Let n x : bs, s')
    S.Read n acc ss
     -> let (bs,s') = bindsOfStatement ss
        in  (Read n acc : bs, s')



-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Scoped n p) where
 pretty s
  = case s of
     If x stmts elses
      -> text "if (" <> pretty x <> text ") "
      <> inner stmts
      <> case elses of
          Block [] [] -> text ""
          _           -> text "else" <> inner elses

     ForeachInts n from to ss
      -> text "foreach ("
      <> pretty n <> text " in " <> pretty from <> text ".." <> pretty to
      <> text ") "
      <> inner ss

     ForeachFacts n vt ss
      -> text "for_facts ("
      <> pretty n <> text " : " <> pretty vt
      <> text ") "
      <> inner ss

     Block [] ss
      -> text "{" <> line
      <> indent 2 (vcat (fmap pretty ss))
      <> line <> text "}"
     Block bs ss
      -> text "{" <> line
      <> indent 2 (vcat (fmap pretty bs))
      <> line
      <> indent 2 (vcat (fmap pretty ss))
      <> line <> text "}"

     Write n x
      -> text "write" <+> pretty n <+> text "=" <+> pretty x
      <> text ";"
     Push  n x
      -> text "push" <+> pretty n <> text "(" <> pretty x <> text ")"
      <> text ";"

     Return x
      -> text "return" <+> pretty x
      <> text ";"

  where
   inner si@(Block _ _) = pretty si
   inner si             = text "{" <> line <> indent 2 (pretty si) <> line <> text "} " <> line


instance (Pretty n, Pretty p) => Pretty (Binding n p) where
 pretty b
  = case b of
     InitAccumulator acc
      -> text "init" <+> pretty acc
      <> text ";"
     Let n x
      -> text "let" <+> pretty n <+> text "=" <+> pretty x
      <> text ";"
     Read n acc
      -> text "read" <+> pretty n <+> text "=" <+> pretty acc
      <> text ";"

