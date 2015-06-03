{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
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
-- import                  Icicle.Internal.Pretty

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

