-- | Query expressions, like aggregates and operators and stuff
-- Expressions can have nested queries, but putting queries and expressions
-- in the same file just gets too confusing.
-- To break the cycle, we make Exp' take a recursive parameter for the query,
-- and "tie the knot" in Query.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Exp (
    Exp'      (..)
  , Prim      (..)
  , Agg       (..)
  , Lit       (..)
  , Op        (..)

  , takeApps
  , takePrimApps
  ) where

import                  Icicle.Source.Query.Operators
import                  Icicle.Internal.Pretty

import                  P


data Exp' q n
 = Var n
 | Nested q
 | App (Exp' q n) (Exp' q n)
 | Prim Prim
 deriving (Show, Eq, Ord)


takeApps :: Exp' q n -> (Exp' q n, [Exp' q n])
takeApps xx
 = case xx of
    App f x
     -> let (f', xs) = takeApps f
        in  (f', xs <> [x])
    _
     -> (xx, [])

takePrimApps :: Exp' q n -> Maybe (Prim, [Exp' q n])
takePrimApps x
 | (Prim p, xs) <- takeApps x
 = Just (p, xs)
 | otherwise
 = Nothing


data Prim
 = Op Op
 | Agg Agg
 | Lit Lit
 deriving (Show, Eq, Ord)

data Agg
 = Count
 -- | Max
 -- | Average
 -- | Sum
 | Newest
 | Oldest
 deriving (Show, Eq, Ord)

data Lit
 = LitInt Int
 deriving (Show, Eq, Ord)



instance (Pretty n, Pretty q) => Pretty (Exp' q n) where
 pretty xx
  = prettyX 0 xx

prettyX :: (Pretty n, Pretty q) => Int -> Exp' q n -> Doc
prettyX outer_prec xx
 = wrap
 $ case xx of
    App{}
     -- Operators
     | Just (Op o, [x]) <- takePrimApps xx
     -> pretty o <+> prettyX inner_prec x
     | Just (Op o, [x,y]) <- takePrimApps xx
     ->  prettyX inner_prec_1 x
     </> pretty  o
     <+> prettyX inner_prec_2 y

    App x y
     ->  prettyX inner_prec_1 x
     <+> prettyX inner_prec_2 y

    Var n
     -> pretty n

    Prim p
     -> pretty p

    Nested q
     -> pretty q

 where
  (inner_prec, assoc) = precedenceOfX xx

  -- Precedence of first operator argument
  inner_prec_1
   | AssocLeft <- assoc
   = inner_prec
   | otherwise
   = inner_prec + 1

  -- Precedence of second operator argument
  inner_prec_2
   | AssocRight <- assoc
   = inner_prec
   | otherwise
   = inner_prec + 1

  -- Suppose we have
  --
  --   7   6   7   (precedences)
  -- a * b + c * d
  --
  --        +        6
  --      /   \
  --     *     *     7
  --    / \   / \
  --   a   b c   d
  --
  -- So when pretty printing, the precedence is allowed to increase
  -- without requiring parentheses, but decreasing needs them.
  --
  wrap
   | inner_prec < outer_prec
   = parens . indent 0
   | otherwise
   = id


-- | Find the pretty-printing precedence of an expression.
precedenceOfX :: Exp' q n -> (Int, Assoc)
precedenceOfX xx
 -- Note: this is assuming that operators will only be applied to one or two arguments,
 -- and that the expression has the right number of arguments.
 | Just (Op o, _) <- takePrimApps xx
 = case fixity o of
    FInfix (Infix a p) -> (p, a)
    FPrefix            -> precedencePrefix
 | otherwise
 = case xx of
    Var{}
     -> precedenceNeverParens
    Nested{}
     -> precedenceAlwaysParens
    App{}
     -> precedenceApplication
    Prim (Op _)
     -> precedenceAlwaysParens
    Prim{}
     -> precedenceNeverParens


instance Pretty Prim where
 pretty (Op o)  = pretty o
 pretty (Agg a) = pretty a
 pretty (Lit l) = pretty l

instance Pretty Agg where
 pretty Count   = "count"
 pretty Newest  = "newest"
 pretty Oldest  = "oldest"

instance Pretty Lit where
 pretty (LitInt i) = text $ show i

