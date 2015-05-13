-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Program (
    Program         (..)
  , Accumulator     (..)
  , AccumulatorType (..)
  , FactLoop        (..)
  , Statement       (..)
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp

import              Icicle.Internal.Pretty

import              P

-- | An entire Avalanche program
data Program n p =
  Program
  { binddate    :: Name n
  , precomps    :: [(Name n, Exp n p)]
  , accums      :: [Accumulator n p]
  , loop        :: FactLoop n p
  , postcomps   :: [(Name n, Exp n p)]
  , returns     :: Exp n p
  }
 deriving (Eq, Ord, Show)


-- | Mutable accumulators
data Accumulator n p
 = Accumulator (Name n)
               (AccumulatorType n p)
 deriving (Eq, Ord, Show)


-- | There are three different kinds of reductions,
-- so three different kinds of accumulators.
data AccumulatorType n p
 -- | Resumable folds, where we store the value for next time
 --
 -- Exp is initial value - only if no history.
 = Resumable ValType (Exp n p)
 -- | Windowed but not latest folds, where for each update we mark
 -- the current fact as necessary for next time
 --
 -- Exp is initial value.
 | Windowed  ValType (Exp n p)
 -- | Latest N, where the value is not so much updated as a
 -- fact is pushed on
 --
 -- Exp is size/count.
 | Latest    ValType (Exp n p)
 deriving (Eq, Ord, Show)


-- | A streaming loop over the inputs.
-- This will be run for every marked value from last execution, plus the new values.
data FactLoop n p =
  FactLoop  ValType             -- ^ What are we iterating over?
            (Name n)            -- ^ What do we call the input?
            [Statement n p]     -- ^ What do we execute?
 deriving (Eq, Ord, Show)

-- | Part of a loop
data Statement n p
 -- Branches
 -- | An IF for filters
 = If (Exp n p)                 [Statement n p]
 -- | Local binding, so the name better be unique
 | Let    (Name n) (Exp n p)    [Statement n p]

 -- Leaf nodes
 -- | Update a resumable or windowed fold accumulator,
 -- with Exp : acc -> acc
 | Update (Name n) (Exp n p)
 -- | Push to a latest accumulator
 -- with Exp : elem
 | Push   (Name n) (Exp n p)
 deriving (Eq, Ord, Show)


instance TransformX Program where
 transformX names exps p
  = do  binddate'  <-      names                   $ binddate  p
        precomps'  <- mapM bind                    $ precomps  p
        accums'    <- mapM (transformX names exps) $ accums    p
        loop'      <-       transformX names exps  $ loop      p
        postcomps' <- mapM bind                    $ postcomps p
        returns'   <-                        exps  $ returns   p
        return $ Program 
               { binddate  = binddate'
               , precomps  = precomps'
               , accums    = accums'
               , loop      = loop'
               , postcomps = postcomps'
               , returns   = returns'
               }
  where
   bind (n,x)
    = do n' <- names n
         x' <- exps x
         return (n', x')

instance TransformX Accumulator where
 transformX names exps (Accumulator n at)
  = do n' <- names n
       case at of
        Resumable t x
         -> Accumulator n' . Resumable t <$> exps x
        Windowed  t x
         -> Accumulator n' . Windowed  t <$> exps x
        Latest    t x
         -> Accumulator n' . Latest    t <$> exps x

instance TransformX FactLoop where
 transformX names exps (FactLoop t bind stmts)
  = FactLoop t
    <$> names bind
    <*> mapM (transformX names exps) stmts

instance TransformX Statement where
 transformX names exps stmt
  = case stmt of
     If x ss
      -> If <$> exps x <*> go ss
     Let n x ss
      -> Let <$> names n <*> exps x <*> go ss
     Update n x
      -> Update <$> names n <*> exps x
     Push n x
      -> Push <$> names n <*> exps x
  where
   go = mapM (transformX names exps)

-- Pretty printing -------------

instance (Pretty n, Pretty p) => Pretty (Program n p) where
 pretty p
  =   text "let " <> pretty (binddate p) <> text " = date; " <> line
  <>  vcat (semis $ fmap prettyX (precomps  p)) <> line
  <>  vcat (semis $ fmap pretty  (accums    p)) <> line
  <>                     pretty  (loop      p)  <> line
  <>  vcat (semis $ fmap prettyX (postcomps p)) <> line
  <>  text "return"  <+> pretty  (returns   p)
  where
   semis = fmap (<> text ";")
   prettyX  (a,b) = pretty a <+> text "=" <+> pretty b


instance (Pretty n, Pretty p) => Pretty (Accumulator n p) where
 pretty (Accumulator n acc)
  =   pretty n <+> text "="
  <+> (case acc of
       Resumable _ x -> pretty x <+> text "(Resumable)"
       Windowed  _ x -> pretty x <+> text "(Windowed)"
       Latest    _ x -> text "Latest" <+> pretty x)


instance (Pretty n, Pretty p) => Pretty (FactLoop n p) where
 pretty (FactLoop t bind stmts)
  =  text "for facts as" <+> pretty bind <+> text ":" <+> pretty t <+> text "{" <> line
  <> indent 2 (semis stmts)     <> line
  <> text "}"
  where
   semis = vcat . fmap (<> text ";") . fmap pretty


instance (Pretty n, Pretty p) => Pretty (Statement n p) where
 pretty s
  = case s of
     If x stmts
      -> text "if (" <> pretty x <> text ") {" <> line
      <> semis stmts
      <> text "}"

     Let n x stmts
      -> text "let" <+> pretty n <+> text "=" <+> pretty x <+> text "in {" <> line
      <> semis stmts
      <> text "}"

     Update n x
      -> text "update" <+> pretty n <+> text "with" <+> pretty x

     Push n x
      -> text "push" <+> pretty n <+> text "with" <+> pretty x

  where
   semis stmts = (indent 2 $ vcat $ fmap (<> text ";") $ fmap pretty stmts) <> line
