-- | Top-level queries
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Query (
    QueryTop  (..)
  , Query     (..)
  , Exp
  , Context

  , simplifyNestedQT
  , simplifyNestedQ
  , simplifyNestedC
  , simplifyNestedX
  ) where

import                  Icicle.Source.Query.Context
import                  Icicle.Source.Query.Exp
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

-- | "Tie the knot" so expressions can have nested queries.
-- See Exp.
type Exp     n = Exp'     (Query n) n
type Context n = Context' (Query n) n

instance Pretty n => Pretty (QueryTop n) where
 pretty q
  =   "feature"   <+> pretty (feature q)
  <> line <> "~>" <+> pretty (query   q)

instance Pretty n => Pretty (Query n) where
 pretty q
  =  cat (fmap (\c -> inp c <> line <> "~> ") (contexts q))
  <> inp                                     (final    q)
  where
  inp p = indent 0 $ pretty p


simplifyNestedQT :: QueryTop n -> QueryTop n
simplifyNestedQT q
 = q { query = simplifyNestedQ $ query q }

simplifyNestedQ :: Query n -> Query n
simplifyNestedQ q
 = Query (fmap simplifyNestedC $ contexts q)
         (     simplifyNestedX $ final    q)

simplifyNestedC :: Context n -> Context n
simplifyNestedC c
 = case c of
    Windowed{} -> c
    Latest{}   -> c
    GroupBy  x -> GroupBy  $ simplifyNestedX x
    Distinct x -> Distinct $ simplifyNestedX x
    Filter   x -> Filter   $ simplifyNestedX x
    LetFold  f -> LetFold f { foldInit = simplifyNestedX $ foldInit f
                            , foldWork = simplifyNestedX $ foldWork f }
    Let   n  x -> Let   n  $ simplifyNestedX x


simplifyNestedX :: Exp n -> Exp n
simplifyNestedX xx
 = case xx of
    Nested (Query [] x)
     -> simplifyNestedX x
    Nested q
     -> Nested $ simplifyNestedQ q

    App x y
     -> App (simplifyNestedX x) (simplifyNestedX y)

    Var{}
     -> xx
    Prim{}
     -> xx

