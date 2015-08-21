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
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Scoped (
    Scoped          (..)
  , Binding         (..)
  , scopedOfStatement
  , statementOfScoped
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
 | ForeachFacts (Name n) (Name n) ValType    S.FactLoopType (Scoped n p)
 | Block                        [Either (Binding n p) (Scoped n p)]
 | Write (Name n) (Exp n p)
 | Push  (Name n) (Exp n p)
 | Output OutputName (Exp n p)
 | KeepFactInHistory
 | LoadResumable (Name n)
 | SaveResumable (Name n)

data Binding n p
 = InitAccumulator (S.Accumulator n p)
 | Let             (Name n) (Exp n p)
 | Read            (Name n) (Name n)

scopedOfStatement :: S.Statement n p -> Scoped n p
scopedOfStatement s
 = case bindsOfStatement s of
    [Right s'] -> s'
    bs         -> Block bs

bindsOfStatement :: S.Statement n p -> [Either (Binding n p) (Scoped n p)]
bindsOfStatement s
 = case s of
    S.If x ss es
     -> [Right $ If x (scopedOfStatement ss) (scopedOfStatement es)]
    S.ForeachInts n from to ss
     -> [Right $ ForeachInts n from to (scopedOfStatement ss)]
    S.ForeachFacts n n' vt lo ss
     -> [Right $ ForeachFacts n n' vt lo (scopedOfStatement ss)]
    S.Block ss
     -- -> fmap (Right . scopedOfStatement) ss
     -> concatMap bindsOfStatement ss
    S.Write n x
     -> [Right $ Write n x]
    S.Push n x
     -> [Right $ Push n x]
    S.Output n x
     -> [Right $ Output n x]
    S.KeepFactInHistory
     -> [Right $ KeepFactInHistory]
    S.LoadResumable n
     -> [Right $ LoadResumable n]
    S.SaveResumable n
     -> [Right $ SaveResumable n]
    S.InitAccumulator acc ss
     -> let bs = bindsOfStatement ss
        in  Left (InitAccumulator acc) : bs
    S.Let n x ss
     -> let bs = bindsOfStatement ss
        in  Left (Let n x) : bs
    S.Read n acc ss
     -> let bs = bindsOfStatement ss
        in  Left (Read n acc) : bs


statementOfScoped :: Scoped n p -> S.Statement n p
statementOfScoped s
 = case s of
    If x ss es
     -> S.If x (statementOfScoped ss) (statementOfScoped es)
    ForeachInts n from to ss
     -> S.ForeachInts n from to (statementOfScoped ss)
    ForeachFacts n n' vt lo ss
     -> S.ForeachFacts n n' vt lo (statementOfScoped ss)
    Block []
     -> S.Block []
    Block bs@(Right _ : _)
     | (ss,bs') <- spanMaybe rightToMaybe bs
     -> let rest = statementOfScoped (Block bs')
            res'
             | S.Block [] <- rest
             = []
             | otherwise
             = [rest]

        in  S.Block (fmap statementOfScoped ss <> res')
    Block (Left b:bs)
     -> let rest = statementOfScoped (Block bs)
        in  case b of
             InitAccumulator acc
              -> S.InitAccumulator acc rest
             Let n x
              -> S.Let n x rest
             Read n acc
              -> S.Read n acc rest

    Write n x
     -> S.Write n x
    Push n x
     -> S.Push n x
    Output n x
     -> S.Output n x
    KeepFactInHistory
     -> S.KeepFactInHistory
    LoadResumable n
     -> S.LoadResumable n
    SaveResumable n
     -> S.SaveResumable n


spanMaybe :: (a -> Maybe b) -> [a] -> ([b],[a])
spanMaybe f as
 = go as
 where
  go [] = ([],[])
  go (a:as')
   = case f a of
      Nothing
       -> ([], a:as')
      Just b
       -> let (bs'',as'') = go as'
          in  (b:bs'', as'')


-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Scoped n p) where
 pretty s
  = case s of
     If x stmts elses
      -> text "if (" <> pretty x <> text ") "
      <> inner stmts
      <> case elses of
          Block [] -> text ""
          _        -> text " else " <> inner elses

     ForeachInts n from to ss
      -> text "foreach ("
      <> pretty n <> text " in " <> pretty from <> text ".." <> pretty to
      <> text ") "
      <> inner ss

     ForeachFacts n n' vt lo ss
      -> text "for_facts ("
      <> pretty n <> text " : " <> pretty vt
      <> text ", "
      <> pretty n' <> text " : Date) in "
      <> pretty lo
      <> text " "
      <> inner ss

     Block bs
      -> text "{" <> line
      <> indent 2 (vcat (fmap (either pretty pretty) bs))
      <> line <> text "}"

     Write n x
      -> text "write" <+> pretty n <+> text "=" <+> pretty x
      <> text ";"
     Push  n x
      -> text "push" <+> pretty n <> text "(" <> pretty x <> text ")"
      <> text ";"

     Output n x
      -> text "output" <+> pretty n <+> pretty x
      <> text ";"
     KeepFactInHistory
      -> text "keep_fact_in_history"
      <> text ";"
     LoadResumable n
      -> text "load_resumable" <+> pretty n
      <> text ";"
     SaveResumable n
      -> text "save_resumable" <+> pretty n
      <> text ";"


  where
   inner si@(Block _) = pretty si
   inner si           = text "{" <> line <> indent 2 (pretty si) <> line <> text "} " <> line


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

