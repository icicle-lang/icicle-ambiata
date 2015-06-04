-- | Top-level queries
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Query (
    QueryTop  (..)
  , Query     (..)
  , Exp
  , Context
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
  =   "feature" <+> pretty (feature q)
  <> line       <>  pretty (query   q)

instance Pretty n => Pretty (Query n) where
 pretty q
  = vcat (fmap (("~>" <+>) . inp) (contexts q))
  <> line
  <>       "~>" <+>    inp  (final    q)
  where
  inp p = indent 0 $ pretty p

