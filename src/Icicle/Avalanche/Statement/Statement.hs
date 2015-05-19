-- | Statements and mutable accumulators (variables) for Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Statement.Statement (
    Statement       (..)
  , Accumulator     (..)
  , AccumulatorType (..)
  , transformUDStmt
  , foldStmt
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              Icicle.Internal.Pretty

import              P


-- | Part of a loop
data Statement n p
 -- Branches
 -- | An IF for filters
 = If (Exp n p)                 (Statement n p) (Statement n p)
 -- | Local binding, so the name better be unique
 | Let    (Name n) (Exp n p)    (Statement n p)

 -- | A loop over some ints
 | ForeachInts (Name n) (Exp n p) (Exp n p)   (Statement n p)

 -- | A loop over all the facts.
 -- This should only occur once in the program, and not inside a loop.
 | ForeachFacts (Name n) ValType (Statement n p)

 -- | Execute several statements in a block.
 | Block                        [Statement n p]

 -- Initialise an accumulator
 | InitAccumulator (Accumulator n p) (Statement n p)

 -- | Read from a non-latest accumulator.
 -- First name is what to call it, second is what accumulator.
 -- As let:
 --      Let  local = accumulator,
 --      Read local = accumulator.
 | Read   (Name n) (Name n)     (Statement n p)

 -- Leaf nodes
 -- | Update a resumable or windowed fold accumulator,
 -- with Exp : acc
 | Write (Name n) (Exp n p)
 -- | Push to a latest accumulator
 -- with Exp : elem
 | Push   (Name n) (Exp n p)

 -- | Return a final value
 | Return (Exp n p)
 deriving (Eq, Ord, Show)

instance Monoid (Statement n p) where
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
data Accumulator n p
 = Accumulator
 { accName      :: Name n
 , accKind      :: AccumulatorType
 , accValType   :: ValType
 , accInit      :: Exp n p
 }
 deriving (Eq, Ord, Show)

-- | There are three different kinds of reductions,
-- each a different kind of accumulator.
-- Additionally, we have non-core accumulators that don't affect history.
data AccumulatorType
 -- | Resumable folds, where we store the value for next time
 --
 -- Exp is initial value - only if no history.
 = Resumable
 -- | Windowed but not latest folds, where for each update we mark
 -- the current fact as necessary for next time
 --
 -- Exp is initial value.
 | Windowed
 -- | Latest N, where the value is not so much updated as a
 -- fact is pushed on
 --
 -- Exp is size/count.
 | Latest

 -- | Another kind of accumulator.
 -- Just a mutable variable with no history.
 | Mutable
 deriving (Eq, Ord, Show)



-- Transforming -------------

transformUDStmt
        :: (Applicative m, Functor m, Monad m)
        => (env -> Statement n p -> m (env, Statement n p))
        -> env
        -> Statement n p
        -> m (Statement n p)
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
          ForeachFacts n ty ss
           -> ForeachFacts n ty <$> go e' ss
          Block ss
           -> Block <$> mapM (go e') ss
          InitAccumulator acc ss
           -> InitAccumulator acc <$> go e' ss
          Read n acc ss
           -> Read n acc <$> go e' ss
          Write n x
           -> return $ Write n x
          Push n x
           -> return $ Push n x
          Return x
           -> return $ Return x

foldStmt
        :: (Applicative m, Functor m, Monad m)
        => (env ->        Statement n p -> m env)
        -> (env -> res -> Statement n p -> m res)
        -> (res -> res -> res)
        -> env
        -> res
        -> Statement n p
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
          ForeachFacts _ _ ss
           -> sub1 ss
          Block ss
           -> do    rs <- mapM (go e') ss
                    let r' = foldl rjoin res rs
                    up e' r' s
          InitAccumulator _ ss
           -> sub1 ss
          Read _ _ ss
           -> sub1 ss
          Write{}
           -> up e' res s
          Push{}
           -> up e' res s
          Return{}
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

     ForeachFacts n v ss
      -> ForeachFacts <$> names n <*> return v <*> go ss

     Block ss
      -> Block <$> gos ss

     InitAccumulator acc ss
      -> InitAccumulator <$> transformX names exps acc <*> go ss

     Read n acc ss
      -> Read <$> names n <*> names acc <*> go ss
     Write n x
      -> Write <$> names n <*> exps x
     Push n x
      -> Push <$> names n <*> exps x

     Return x
      -> Return <$> exps x

  where
   go  = transformX names exps
   gos = mapM go


instance TransformX Accumulator where
 transformX names exps (Accumulator n at t x)
  = do n' <- names n
       x' <- exps  x
       return $ Accumulator n' at t x'


-- Pretty printing -------------


instance (Pretty n, Pretty p) => Pretty (Statement n p) where
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

     Read n acc stmts
      -> text "read" <+> pretty n <+> text "=" <+> pretty acc <> line
      <> semisScope stmts

     ForeachInts n from to stmts
      -> text "for" <+> pretty n <+> text "in" <+> pretty from <+> text ".." <+> pretty to <> line
      <> semis stmts

     ForeachFacts n t stmts
      -> text "for facts as" <+> pretty n <+> text ":" <+> pretty t <> line
      <> semis stmts

     Block stmts
      -- We don't actually need to indent here,
      -- because it doesn't really introduce scope
      -> vcat $ fmap pretty stmts

     InitAccumulator acc stmts
      -> pretty acc <> line
      <> semisScope stmts

     Write n x
      -> text "update" <+> pretty n <+> text "=" <+> pretty x <> line

     Push n x
      -> text "push" <+> pretty n <+> text "=" <+> pretty x

     Return x
      -> text "return" <+> pretty x

  where
   semis stmt
    = indent 2 $ pretty stmt

   -- We don't want to indent for every let or read just for aesthetic reasons:
   -- it gets messy very quickly
   semisScope stmt
    = indent (inde stmt) $ pretty stmt

   inde Block{} = 2
   inde _       = 0


instance (Pretty n, Pretty p) => Pretty (Accumulator n p) where
 pretty (Accumulator n acc _ x)
  =   pretty n <+> text "=" <+> pretty x
  <+> (case acc of
       Resumable -> text "(Resumable)"
       Windowed  -> text "(Windowed)"
       Latest    -> text "(Latest)"
       Mutable   -> text "(Mutable)")

