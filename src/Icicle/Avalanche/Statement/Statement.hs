-- | Statements and mutable accumulators (variables) for Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.Statement.Statement (
    Statement       (..)
  , Accumulator     (..)
  , FactLoopType    (..)
  , transformUDStmt
  , foldStmt
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              Icicle.Internal.Pretty

import              P


-- | Part of a loop
data Statement a n p
 -- Branches
 -- | An IF for filters
 = If           !(Exp a n p) !(Statement a n p) !(Statement a n p)
 -- | Local binding, so the name better be unique
 | Let !(Name n) !(Exp a n p) !(Statement a n p)

 -- | A loop over some ints
 | ForeachInts  !(Name n) !(Exp a n p) !(Exp a n p) !(Statement a n p)

 -- | A loop over all the facts.
 -- This should only occur once in the program, and not inside a loop.
 | ForeachFacts ![(Name n, ValType)] !ValType !FactLoopType !(Statement a n p)

 -- | Execute several statements in a block.
 | Block ![Statement a n p]

 -- Initialise an accumulator
 | InitAccumulator !(Accumulator a n p) !(Statement a n p)

 -- | Read from a non-latest accumulator.
 -- First name is what to call it, second is what accumulator.
 -- As let:
 --      Let  local = accumulator,
 --      Read local = accumulator.
 | Read   !(Name n) !(Name n) !ValType !(Statement a n p)

 -- Leaf nodes
 -- | Update a resumable or windowed fold accumulator,
 -- with Exp : acc
 | Write  !(Name n) !(Exp a n p)

 -- | Emit a value to output
 | Output !OutputName !ValType ![(Exp a n p, ValType)]

 -- | Mark the current fact as being historically relevant
 | KeepFactInHistory

 -- | Load an accumulator from history. Must be before any fact loops.
 | LoadResumable !(Name n) !ValType

 -- | Save an accumulator to history. Must be after all fact loops.
 | SaveResumable !(Name n) !ValType
 deriving (Eq, Ord, Show)

instance Monoid (Statement a n p) where
 mempty = Block []

 mappend (Block ps) (Block qs)
        = Block (ps <> qs)
 mappend (Block ps) q
        = Block (ps <> [q])
 mappend p (Block qs)
        = Block (p : qs)
 mappend p q
        = Block [p, q]


-- | Mutable accumulators
data Accumulator a n p
 = Accumulator
 { accName      :: !(Name n)
 , accValType   :: !ValType
 , accInit      :: !(Exp a n p)
 }
 deriving (Eq, Ord, Show)


-- | When executing the feature, we also keep track of what data
-- will be required to compute the next snapshot.
-- This consists of the list of individual facts that contribute
-- to "windowed" and "latest" features,
-- as well as the last values of any resumable features like reductions.
--
-- It is important that since the resumable features have already seen the
-- historical data, they cannot see it again.
-- This is why we have two separate loops, so the first loop over historical data
-- does not compute the resumable features:
--
-- 1. Initialise variables for latest and windowed features
-- 2. Loop through historical data, computing latest and windowed features
-- 3. Read last values of resumable variables
-- 4. Loop through new data, computing all features
-- 5. Store last values of resumable variables
-- 6. Return
--
data FactLoopType
 -- | Loop over the facts that contributed to the last snapshot's windowed and latest features
 = FactLoopHistory
 -- | Loop over newly added facts since the last snapshot
 | FactLoopNew
 deriving (Eq, Ord, Show)


-- Transforming -------------

transformUDStmt
        :: (Applicative m, Functor m, Monad m)
        => (env -> Statement a n p -> m (env, Statement a n p))
        -> env
        -> Statement a n p
        -> m (Statement a n p)
transformUDStmt fun env statements
 = go env statements
 where
  go e s
   = do  (e', s') <- fun e s
         case s' of
          If x ss es
           -> If x <$> go e' ss <*> go e' es
          Let n x ss
           -> Let n x <$> go e' ss
          ForeachInts n from to ss
           -> ForeachInts n from to <$> go e' ss
          ForeachFacts ns ty lo ss
           -> ForeachFacts ns ty lo <$> go e' ss
          Block ss
           -> Block <$> mapM (go e') ss
          InitAccumulator acc ss
           -> InitAccumulator acc <$> go e' ss
          Read n acc vt ss
           -> Read n acc vt <$> go e' ss
          Write n x
           -> return $ Write n x
          Output n t xs
           -> return $ Output n t xs
          KeepFactInHistory
           -> return $ KeepFactInHistory
          LoadResumable n t
           -> return $ LoadResumable n t
          SaveResumable n t
           -> return $ SaveResumable n t

foldStmt
        :: (Applicative m, Functor m, Monad m)
        => (env ->        Statement a n p -> m env)
        -> (env -> res -> Statement a n p -> m res)
        -> (res -> res -> res)
        -> env
        -> res
        -> Statement a n p
        -> m res
foldStmt down up rjoin env res statements
 = go env statements
 where
  go e s
   = do  e' <- down e s
         let sub1 ss = go e' ss >>= \r' -> up e' r' s

         case s of
          If _ ss es
           -> do    r1 <- go e' ss
                    r2 <- go e' es
                    let r' = rjoin r1 r2
                    up e' r' s
          Let _ _ ss
           -> sub1 ss
          ForeachInts _ _ _ ss
           -> sub1 ss
          ForeachFacts _ _ _ ss
           -> sub1 ss
          Block ss
           -> do    rs <- mapM (go e') ss
                    let r' = foldl rjoin res rs
                    up e' r' s
          InitAccumulator _ ss
           -> sub1 ss
          Read _ _ _ ss
           -> sub1 ss
          Write{}
           -> up e' res s
          Output{}
           -> up e' res s
          KeepFactInHistory
           -> up e' res s
          LoadResumable{}
           -> up e' res s
          SaveResumable{}
           -> up e' res s



instance TransformX Statement where
 transformX names exps stmt
  = case stmt of
     If x ss es
      -> If <$> exps x <*> go ss <*> go es
     Let n x ss
      -> Let <$> names n <*> exps x <*> go ss

     ForeachInts n from to ss
      -> ForeachInts <$> names n <*> exps from <*> exps to <*> go ss

     ForeachFacts ns v lo ss
      -> let name_go (n, t) = (,) <$> names n <*> pure t
         in ForeachFacts <$> traverse name_go ns <*> return v <*> return lo <*> go ss

     Block ss
      -> Block <$> gos ss

     InitAccumulator acc ss
      -> InitAccumulator <$> transformX names exps acc <*> go ss

     Read n acc vt ss
      -> Read <$> names n <*> names acc <*> pure vt <*> go ss
     Write n x
      -> Write <$> names n <*> exps x

     Output n ty xs
      -> Output n ty <$> traverse (\(x,t) -> (,) <$> exps x <*> pure t) xs

     KeepFactInHistory
      -> return KeepFactInHistory

     LoadResumable n t
      -> LoadResumable <$> names n <*> pure t
     SaveResumable n t
      -> SaveResumable <$> names n <*> pure t

  where
   go  = transformX names exps
   gos = mapM go


instance TransformX Accumulator where
 transformX names exps (Accumulator n t x)
  = do n' <- names n
       x' <- exps  x
       return $ Accumulator n' t x'


-- Pretty printing -------------


instance (Pretty n, Pretty p) => Pretty (Statement a n p) where
 pretty s
  = case s of
     If x stmts elses
      -> text "if (" <> pretty x <> text "):" <> line
      <> semis stmts
      <> case elses of
          Block [] -> text ""
          _        -> line <> text "else" <> line <> semis elses

     Let n x stmts
      -> pretty n <+> text "=" <+> pretty x <> line
      <> semisScope stmts

     Read n acc vt stmts
      -> text "read" <+> pretty n <+> text "=" <+> pretty acc
                                               <+> brackets (pretty vt) <> line
      <> semisScope stmts

     ForeachInts n from to stmts
      -> text "for" <+> pretty n <+> text "in" <+> pretty from <+> text ".." <+> pretty to <> line
      <> semis stmts

     ForeachFacts ns t lo stmts
      -> text "for facts : [" <> pretty t <> text "] as" <+> prettyFactParts ns <+> text "in" <+> pretty lo <> line
      <> semis stmts

     Block stmts
      -- We don't actually need to indent here,
      -- because it doesn't really introduce scope
      -> vcat $ fmap pretty stmts

     InitAccumulator acc stmts
      -> text "init" <+> pretty acc <> line
      <> semisScope stmts

     Write n x
      -> text "write" <+> pretty n <+> text "=" <+> pretty x <> line

     Output n t xs
      -> text "output" <+> pretty n <+> pretty t <+> pretty xs

     KeepFactInHistory
      -> text "keep_fact_in_history"

     LoadResumable n t
      -> annotate (AnnType t) "load_resumable" <+> pretty n
     SaveResumable n t
      -> annotate (AnnType t) "save_resumable" <+> pretty n


  where
   semis stmt
    = indent 2 $ pretty stmt

   -- We don't want to indent for every let or read just for aesthetic reasons:
   -- it gets messy very quickly
   semisScope stmt
    = indent (inde stmt) $ pretty stmt

   inde Block{} = 2
   inde _       = 0

   prettyFactPart (nf, tf) = pretty nf <+> text ":" <+> pretty tf
   prettyFactParts         = parens . align . cat . punctuate comma . fmap prettyFactPart


instance (Pretty n, Pretty p) => Pretty (Accumulator a n p) where
 pretty (Accumulator n vt x)
  = annotate (AnnType (pretty vt)) (pretty n) <+> text "=" <+> pretty x

instance Pretty FactLoopType where
 pretty FactLoopHistory = text "history"
 pretty FactLoopNew     = text "new"

