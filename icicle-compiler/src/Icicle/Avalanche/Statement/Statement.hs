-- | Statements and mutable accumulators (variables) for Avalanche

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.Statement.Statement (
    Statement       (..)
  , Accumulator     (..)
  , FactBinds       (..)
  , ForeachType     (..)
  , WhileType       (..)
  , nestedIfs
  , transformUDStmt
  , foldStmt
  , factBindsAll
  ) where

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Exp

import           Icicle.Data.Name

import           Icicle.Internal.Pretty

import           P


-- | Part of a loop
data Statement a n p
 -- Branches
 -- | An IF for filters
 = If !(Exp a n p) (Statement a n p) (Statement a n p)
 -- | Local binding, so the name better be unique
 | Let {-# UNPACK #-} !(Name n) !(Exp a n p) (Statement a n p)

 -- | A loop with some condition on an accumulator.
 | While !WhileType {-# UNPACK #-} !(Name n) !ValType !(Exp a n p) (Statement a n p)

 -- | A loop over some ints
 | ForeachInts !ForeachType {-# UNPACK #-} !(Name n) !(Exp a n p) !(Exp a n p) (Statement a n p)

 -- | A loop over all the facts.
 -- This should only occur once in the program, and not inside a loop.
 | ForeachFacts !(FactBinds n) !ValType (Statement a n p)

 -- | Execute several statements in a block.
 | Block [Statement a n p]

 -- Initialise an accumulator
 | InitAccumulator !(Accumulator a n p) (Statement a n p)

 -- | Read from a non-latest accumulator.
 -- First name is what to call it, second is what accumulator.
 -- As let:
 --      Let  local = accumulator,
 --      Read local = accumulator.
 | Read {-# UNPACK #-} !(Name n) {-# UNPACK #-} !(Name n) !ValType (Statement a n p)

 -- Leaf nodes
 -- | Update a resumable or windowed fold accumulator,
 -- with Exp : acc
 | Write {-# UNPACK #-} !(Name n) !(Exp a n p)

 -- | Emit a value to output
 | Output {-# UNPACK #-} !OutputId !ValType ![(Exp a n p, ValType)]

 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n, NFData p) => NFData (Statement a n p)

instance Monoid (Statement a n p) where
 mempty = Block []
 mappend p q
        = Block [p, q]

-- | Construct nested ifs. Use this instead of "If (x && y)", since
--   A-normalisation will get rid of the short-circuit.
--
nestedIfs :: [Exp a n p] -> Statement a n p -> Statement a n p -> Statement a n p
nestedIfs [] _ _
 = mempty
nestedIfs conds true false
 = foldr (\cond st -> If cond st false) true conds

data FactBinds n
 = FactBinds {
    factBindTime    :: !(Name n)
  , factBindValue   :: ![(Name n, ValType)]
 }
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (FactBinds n)

factBindsAll :: FactBinds n -> [(Name n, ValType)]
factBindsAll (FactBinds ntime nvalue)
 = (ntime, TimeT) : nvalue

-- | Mutable accumulators
data Accumulator a n p
 = Accumulator
 { accName      :: !(Name n)
 , accValType   :: !ValType
 , accInit      :: !(Exp a n p)
 }
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n, NFData p) => NFData (Accumulator a n p)

data ForeachType
 = ForeachStepUp
 | ForeachStepDown
 deriving (Eq, Ord, Show, Generic)

instance NFData ForeachType

data WhileType
 = WhileEq
 | WhileNe
 deriving (Eq, Ord, Show, Generic)

instance NFData WhileType

-- Transforming -------------

transformUDStmt
        :: Monad m
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
          While t n vt end ss
           -> While t n vt end <$> go e' ss
          ForeachInts t n from to ss
           -> ForeachInts t n from to <$> go e' ss
          ForeachFacts binds ty ss
           -> ForeachFacts binds ty <$> go e' ss
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
{-# INLINE transformUDStmt #-}


foldStmt
        :: Monad m
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
          While _ _ _ _ ss
           -> sub1 ss
          ForeachInts _ _ _ _ ss
           -> sub1 ss
          ForeachFacts _ _ ss
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
{-# INLINE foldStmt #-}


instance TransformX Statement where
 transformX names exps stmt
  = case stmt of
     If x ss es
      -> If <$> exps x <*> go ss <*> go es
     Let n x ss
      -> Let <$> names n <*> exps x <*> go ss

     While t n vt end ss
      -> While t <$> names n <*> pure vt <*> exps end <*> go ss
     ForeachInts t n from to ss
      -> ForeachInts t <$> names n <*> exps from <*> exps to <*> go ss

     ForeachFacts (FactBinds ntime ns) v ss
      -> let name_go (n, t) = (,) <$> names n <*> pure t
         in ForeachFacts <$> (FactBinds <$> names ntime <*> traverse name_go ns) <*> return v <*> go ss

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

  where
   go  = transformX names exps
   gos = mapM go


instance TransformX Accumulator where
 transformX names exps (Accumulator n t x)
  = do n' <- names n
       x' <- exps  x
       return $ Accumulator n' t x'


-- Pretty printing -------------

flattenBlocks :: Statement a n p -> [Statement a n p]
flattenBlocks = \case
  Block xs ->
    concatMap flattenBlocks xs
  x ->
    [x]

instance (Pretty n, Pretty p) => Pretty (Statement a n p) where
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

    Let n x stmts ->
      annotate AnnBinding (pretty n) <+> "=" <+> pretty x <> line <>
      nosubscope stmts

    Read n acc _vt stmts ->
      annotate AnnBinding (pretty n) <+> prettyPunctuation "=r" <+> annotate AnnVariable (pretty acc) <> line <>
      nosubscope stmts

    Write n x ->
      annotate AnnBinding (pretty n) <+> prettyPunctuation "=w" <+> pretty x

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
        prettyFactParts AnnBinding (factBindsAll binds) <+>
      subscope stmts <>
      line

    x@(Block _) ->
     -- We don't actually need to indent here,
     -- because it doesn't really introduce scope
     vcat (fmap pretty $ flattenBlocks x)

    InitAccumulator (Accumulator n vt x) stmts ->
      annotate AnnBinding (pretty n) <+> prettyPunctuation "=i" <+> prettyTypedFlat (pretty x) (pretty vt) <> line <>
      nosubscope stmts

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

instance Pretty WhileType where
  pretty = \case
    WhileEq ->
      prettyPunctuation "=="
    WhileNe ->
      prettyPunctuation "!="

instance (Pretty n, Pretty p) => Pretty (Accumulator a n p) where
  pretty (Accumulator n vt x) =
    prettyTypedFlat (annotate AnnBinding $ pretty n) (pretty vt) <+> text "=" <+> pretty x

