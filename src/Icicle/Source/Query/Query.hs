-- | Top-level queries
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Query.Query (
    QueryTop  (..)
  , Query     (..)
  , Exp
  , Context

  , simplifyNestedQT
  , simplifyNestedQ
  , simplifyNestedC
  , simplifyNestedX

  , reannotQT
  , reannotQ
  , reannotC
  , reannotX
  , annotOfQuery
  ) where

import                  Icicle.Source.Query.Context
import                  Icicle.Source.Query.Exp
import                  Icicle.Internal.Pretty
import                  Icicle.Common.Base

import                  P


data QueryTop a n
 = QueryTop
 { feature   :: Name n
 , queryName :: OutputName
 , query     :: Query a n }
 deriving (Show, Eq, Ord)

data Query a n
 = Query
 { contexts :: [Context a n]
 , final    :: Exp a n }
 deriving (Show, Eq, Ord)

-- | "Tie the knot" so expressions can have nested queries.
-- See Exp.
type Exp     a n = Exp'     (Query a n) a n
type Context a n = Context' (Query a n) a n

instance Pretty n => Pretty (QueryTop a n) where
 pretty q
  =   "feature"   <+> pretty (feature q)
  <> line <> "~>" <+> pretty (query   q)

instance Pretty n => Pretty (Query a n) where
 pretty q
  =  cat (fmap (\c -> inp c <> line <> "~> ") (contexts q))
  <> inp                                     (final    q)
  where
  inp p = indent 0 $ pretty p


simplifyNestedQT :: QueryTop a n -> QueryTop a n
simplifyNestedQT q
 = q { query = simplifyNestedQ $ query q }

simplifyNestedQ :: Query a n -> Query a n
simplifyNestedQ q
 = simp
 $ Query (fmap simplifyNestedC $ contexts q)
         (     simplifyNestedX $ final    q)
 where
  simp q'
   | Query cs (Nested _ (Query cs' x')) <- q'
   = Query (cs <> cs') x'
   | otherwise
   = q'

simplifyNestedC :: Context a n -> Context a n
simplifyNestedC c
 = case c of
    Windowed{}  -> c
    Latest{}     -> c
    GroupBy  a x -> GroupBy  a $ simplifyNestedX x
    GroupFold a k v x -> GroupFold a k v $ simplifyNestedX x
    Distinct a x -> Distinct a $ simplifyNestedX x
    Filter   a x -> Filter   a $ simplifyNestedX x
    LetFold  a f -> LetFold a f { foldInit = simplifyNestedX $ foldInit f
                                , foldWork = simplifyNestedX $ foldWork f }
    Let a n  x -> Let a n  $ simplifyNestedX x


simplifyNestedX :: Exp a n -> Exp a n
simplifyNestedX xx
 = case xx of
    Nested _ (Query [] x)
     -> simplifyNestedX x
    Nested a q
     -> Nested a $ simplifyNestedQ q

    App a x y
     -> App a (simplifyNestedX x) (simplifyNestedX y)

    Var{}
     -> xx
    Prim{}
     -> xx

    Case a scrut pats
     -> Case a (simplifyNestedX scrut)
      $ fmap (\(p,x) -> (p, simplifyNestedX x)) pats



reannotX :: (a -> a') -> Exp a n -> Exp a' n
reannotX f xx
 = case xx of
    Var    a n   -> Var    (f a)  n
    Nested a q   -> Nested (f a) (reannotQ f q)
    App    a x y -> App    (f a) (reannotX f x) (reannotX f y)
    Prim   a p   -> Prim   (f a)  p
    Case a scrut pats
     -> Case (f a) (reannotX f scrut)
      $ fmap (\(p,x) -> (p, reannotX f x)) pats


reannotC :: (a -> a') -> Context a n -> Context a' n
reannotC f cc
 = case cc of
    Windowed  a b c d -> Windowed  (f a) b c d
    Latest    a i     -> Latest    (f a) i
    GroupBy   a x     -> GroupBy   (f a) (reannotX f x)
    GroupFold a k v x -> GroupFold (f a) k v (reannotX f x)
    Distinct  a x     -> Distinct  (f a) (reannotX f x)
    Filter    a x     -> Filter    (f a) (reannotX f x)
    LetFold   a ff    -> LetFold   (f a) (ff { foldInit = reannotX f (foldInit ff)
                                             , foldWork = reannotX f (foldWork ff) })
    Let      a n x    -> Let       (f a) n (reannotX f x)

reannotQ :: (a -> a') -> Query a n -> Query a' n
reannotQ f q
 = q
 { contexts = fmap (reannotC f) (contexts q)
 , final    =       reannotX f  (final    q)
 }

reannotQT :: (a -> a') -> QueryTop a n -> QueryTop a' n
reannotQT f qt
 = qt
 { query = reannotQ f (query qt) }


annotOfQuery :: Query a n -> a
annotOfQuery q
 = case contexts q of
    []    -> annotOfExp $ final q
    (c:_) -> annotOfContext c

