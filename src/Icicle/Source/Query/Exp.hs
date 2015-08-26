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
  , Lit       (..)
  , Op        (..)
  , Fun       (..)

  , takeApps
  , takePrimApps
  , annotOfExp
  , mkApp
  ) where

import                  Icicle.Source.Query.Constructor
import                  Icicle.Source.Query.Operators
import                  Icicle.Internal.Pretty
import                  Icicle.Common.Base

import                  P

import                  Data.Text (Text)


data Exp' q a n
 = Var a (Name n)
 | Nested a q
 | App  a (Exp' q a n) (Exp' q a n)
 | Prim a Prim
 | Case a (Exp' q a n) [(Pattern n, Exp' q a n)]
 deriving (Show, Eq, Ord)


takeApps :: Exp' q a n -> (Exp' q a n, [Exp' q a n])
takeApps xx
 = case xx of
    App _ f x
     -> let (f', xs) = takeApps f
        in  (f', xs <> [x])
    _
     -> (xx, [])

takePrimApps :: Exp' q a n -> Maybe (Prim, a, [Exp' q a n])
takePrimApps x
 | (Prim a p, xs) <- takeApps x
 = Just (p, a, xs)
 | otherwise
 = Nothing

annotOfExp :: Exp' q a n -> a
annotOfExp x
 = case x of
   Var    a _   -> a
   Nested a _   -> a
   App    a _ _ -> a
   Prim   a _   -> a
   Case   a _ _ -> a

mkApp :: Exp' q a n -> Exp' q a n -> Exp' q a n
mkApp x y
 = App (annotOfExp x) x y


data Prim
 = Op Op
 | Lit Lit
 | Fun Fun
 | PrimCon Constructor
 deriving (Show, Eq, Ord)

data Lit
 = LitInt Int
 | LitDouble Double
 | LitString Text
 deriving (Show, Eq, Ord)

data Fun
 = Log
 | Exp
 | ToDouble
 | ToInt
 deriving (Show, Eq, Ord)


instance (Pretty n, Pretty q) => Pretty (Exp' q a n) where
 pretty xx
  = prettyX 0 xx

prettyX :: (Pretty n, Pretty q) => Int -> Exp' q a n -> Doc
prettyX outer_prec xx
 = wrap
 $ case xx of
    App{}
     -- Operators
     | Just (Op o, _, [x]) <- takePrimApps xx
     , FPrefix <- fixity o
     -> pretty o <+> prettyX inner_prec x
     | Just (Op o, _, [x,y]) <- takePrimApps xx
     , FInfix _ <- fixity o
     ->  prettyX inner_prec_1 x
     </> pretty  o
     <+> prettyX inner_prec_2 y

    App _ x y
     ->  prettyX inner_prec_1 x
     <+> prettyX inner_prec_2 y

    Var _ n
     -> pretty n

    Prim _ p
     -> pretty p

    Nested _ q
     -> pretty q

    Case _ scrut pats
     -> indent 0
     (  "case" <+> pretty scrut <> line
     <> indent 2 (vcat $ fmap (\(p,x) -> "| " <> pretty p <> " -> " <> pretty x) pats) <> line
     <> "end")

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
precedenceOfX :: Exp' q a n -> (Int, Assoc)
precedenceOfX xx
 -- Note: this is assuming that operators will only be applied to one or two arguments,
 -- and that the expression has the right number of arguments.
 | Just (Op o, _, as) <- takePrimApps xx
 = case fixity o of
    FInfix (Infix a p)
     | length as == 2
     -> (p, a)
    FPrefix 
     | length as == 1
     -> precedencePrefix

    _
     -> precedenceApplication
 | otherwise
 = case xx of
    Var{}
     -> precedenceNeverParens
    Nested{}
     -> precedenceAlwaysParens
    App{}
     -> precedenceApplication
    Prim _ (Op _)
     -> precedenceAlwaysParens
    Prim{}
     -> precedenceNeverParens
    Case{}
     -> precedenceApplication


instance Pretty Prim where
 pretty (Op o)  = pretty o
 pretty (Lit l) = pretty l
 pretty (Fun f) = pretty f
 pretty (PrimCon c) = pretty c

instance Pretty Lit where
 pretty (LitInt i) = text $ show i
 pretty (LitDouble i) = text $ show i
 pretty (LitString i) = text $ show i

instance Pretty Fun where
 pretty Log         = "log"
 pretty Exp         = "exp"
 pretty ToDouble    = "double"
 pretty ToInt       = "int"

