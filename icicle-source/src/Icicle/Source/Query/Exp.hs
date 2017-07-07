-- | Query expressions, like aggregates and operators and stuff
-- Expressions can have nested queries, but putting queries and expressions
-- in the same file just gets too confusing.
-- To break the cycle, we make Exp' take a recursive parameter for the query,
-- and "tie the knot" in Query.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Query.Exp (
    Exp'      (..)
  , Prim      (..)
  , Lit       (..)
  , Op        (..)
  , Fun
  , BuiltinFun   (..)
  , BuiltinMath  (..)
  , BuiltinTime  (..)
  , BuiltinData  (..)
  , BuiltinArray (..)
  , BuiltinMap   (..)

  , takeApps
  , takePrimApps
  , annotOfExp
  , mkApp
  , precedenceOfX
  , listOfBuiltinFuns
  ) where

import                  Icicle.Source.Query.Builtin
import                  Icicle.Source.Query.Constructor
import                  Icicle.Source.Query.Operators
import                  Icicle.Data.Time
import                  Icicle.Internal.Pretty
import                  Icicle.Common.Base

import                  P

import                  Data.Text (unpack)


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
 | LitTime Time
 deriving (Show, Eq, Ord)

-- | Built-in Source functions
type Fun = BuiltinFun

instance (Pretty n, Pretty q) => Pretty (Exp' q a n) where
  prettyPrec outer_prec xx =
    wrap $
      case xx of
        App{}
         -- Operators
         | Just (Op o, _, [x]) <- takePrimApps xx
         , FPrefix <- fixity o
         -> pretty o <+> prettyPrec inner_prec x
         | Just (Op o, _, [x,y]) <- takePrimApps xx
         , FInfix _ <- fixity o
         ->  prettyPrec inner_prec_1 x
         <+> pretty  o
         <+> prettyPrec inner_prec_2 y

        App _ x y ->
          prettyPrec inner_prec_1 x <+> prettyPrec inner_prec_2 y

        Var _ n ->
          annotate AnnVariable (pretty n)

        Prim _ p ->
          annotate AnnPrimitive (pretty p)

        Nested _ q ->
          pretty q

        Case _ scrut pats ->
          vsep [
              prettyKeyword "case" <+> pretty scrut
            , vcat . with pats $ \(p, x) ->
                vsep [
                    prettyPunctuation "|" <+> pretty p <+> prettyPunctuation "->"
                  , indent 4 $ pretty x
                  ]
            , prettyKeyword "end"
            ]
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
    wrap =
      parensWhen (inner_prec < outer_prec)

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
  pretty = \case
    Op o ->
      pretty o
    Lit l ->
      pretty l
    Fun f ->
      pretty f
    PrimCon c ->
      pretty c

instance Pretty Lit where
  pretty = \case
    LitInt i ->
      annotate AnnConstant . text $ show i
    LitDouble i ->
      annotate AnnConstant . text $ show i
    LitString i ->
      annotate AnnConstant . text $ show i
    LitTime i ->
      annotate AnnConstant $
        "`" <> (text $ unpack $ renderTime i) <> "`"
