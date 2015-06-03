{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query (
    QueryTop  (..)
  , Query     (..)
  , Context   (..)
  , WindowUnit(..)
  , Fold      (..)
  , FoldType  (..)
  , Exp       (..)
  , Prim      (..)
  , Agg       (..)
  , Op        (..)
  , Sort      (..)

  , takeApps
  , takePrimApps
  ) where

import                  Icicle.Source.Query.Operators
import                  Icicle.Internal.Pretty

import                  P


data QueryTop n
 = QueryTop
 { feature  :: n
 , query    :: Query n }
 deriving (Show, Eq, Ord)

data Query n
 = Query
 { contexts :: [Context n] 
 , final    :: Exp n }
 deriving (Show, Eq, Ord)

data Context n
 = Windowed WindowUnit (Maybe WindowUnit)
 | Latest Int
 | GroupBy  (Exp n)
 | Distinct (Exp n)
 | Filter   (Exp n)
 | LetFold  (Fold n)
 | Let  (Maybe Sort) n   (Exp n)
 deriving (Show, Eq, Ord)

data WindowUnit
 = Days Int
 | Months Int
 | Weeks Int
 deriving (Show, Eq, Ord)

data Fold n
 = Fold
 { foldBind :: n
 , foldInit :: Exp n
 , foldWork :: Exp n
 , foldType :: FoldType }
 deriving (Show, Eq, Ord)

data FoldType
 = FoldTypeFoldl1
 -- | FoldTypeFoldl
 deriving (Show, Eq, Ord)

data Sort = Stream | Aggregate
 deriving (Show, Eq, Ord)


data Exp n
 = Var n
 | Nested (Query n)
 | App (Exp n) (Exp n)
 | Prim Prim
 deriving (Show, Eq, Ord)

takeApps :: Exp n -> (Exp n, [Exp n])
takeApps xx
 = case xx of
    App f x
     -> let (f', xs) = takeApps f
        in  (f', xs <> [x])
    _
     -> (xx, [])

takePrimApps :: Exp n -> Maybe (Prim, [Exp n])
takePrimApps x
 | (Prim p, xs) <- takeApps x
 = Just (p, xs)
 | otherwise
 = Nothing


data Prim
 = Op Op
 | Agg Agg
 deriving (Show, Eq, Ord)

data Agg
 = Count
 -- | Max
 -- | Average
 -- | Sum
 | Newest
 | Oldest
 deriving (Show, Eq, Ord)


instance Pretty n => Pretty (QueryTop n) where
 pretty q
  =   "feature" <+> pretty (feature q)
  <> line       <>  pretty (query   q)

instance Pretty n => Pretty (Query n) where
 pretty q
  = vcat (fmap (("~>" <+>) . inp) (contexts q))
  <> line
  <>       "~>" <+>    inp  (final    q)
  where
  inp p = indent 0 $ pretty p


instance Pretty n => Pretty (Context n) where
 pretty cc
  = case cc of
     Windowed newer Nothing
      -> "windowed" <+> pretty newer
     Windowed newer (Just older)
      -> "windowed between" <+> pretty older
                  <+> "and" <+> pretty newer

     Latest   i
      -> "latest"   <+> pretty i
     GroupBy  x
      -> "group"    <+> pretty x
     Distinct x
      -> "distinct" <+> pretty x
     Filter   x
      -> "filter"   <+> pretty x
     LetFold f
      ->  "let fold"
      <+> pretty (foldBind f)
      <+> "="
      <+> pretty (foldInit f)
      <+> ":"
      <+> pretty (foldWork f)

     Let _ b x
      ->  "let"
      <+> pretty b
      <+> "="
      <+> pretty x

     
instance Pretty WindowUnit where
 pretty wu
  = case wu of
     Days   i -> pretty i <+> "days"
     Months i -> pretty i <+> "months"
     Weeks  i -> pretty i <+> "weeks"

instance Pretty n => Pretty (Exp n) where
 pretty xx
  = prettyX 0 xx

prettyX :: Pretty n => Int -> Exp n -> Doc
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
precedenceOfX :: Exp n -> (Int, Assoc)
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

instance Pretty Agg where
 pretty Count   = "count"
 pretty Newest  = "newest"
 pretty Oldest  = "oldest"

