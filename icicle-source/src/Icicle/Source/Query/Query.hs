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

  , allvarsX, allvarsC, allvarsQ
  ) where

import                  Icicle.Common.Base
import                  Icicle.Data.Name
import                  Icicle.Internal.Pretty
import                  Icicle.Source.Query.Constructor
import                  Icicle.Source.Query.Context
import                  Icicle.Source.Query.Exp

import qualified        Data.Set                as Set

import                  P


data QueryTop a n
 = QueryTop
 { queryInput :: UnresolvedInputId
 , queryName  :: OutputId
 , query      :: Query a n }
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
  pretty q =
    vsep [
        prettyKeyword "feature" <+> annotate AnnConstant (pretty (show (renderUnresolvedInputId (queryInput q))))
      , prettyPunctuation "~>" <+> align (pretty (query q))
      ]

instance Pretty n => Pretty (Query a n) where
  pretty q =
    align . prettyItems vsep (align . pretty $ final q) $
      fmap (PrettyItem (prettyPunctuation "~>") . align . pretty) (contexts q)

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
    Windowed  a b c   -> Windowed  (f a) b c
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


-- | Compute set of all value variables of queries
-- (As opposed to type variables; for that see freeOfAllQ in Checker/Constraint)
allvarsQ :: Eq n => Query a n -> Query (a, Set.Set (Name n)) n
allvarsQ (Query cs x)
 = let x'  = allvarsX x
       cs' = goCs x'
   in  Query cs' x'
 where
  goCs x'
   = reverse
   $ goCs' (snd $ annotOfExp x')
   $ reverse cs

  goCs' _ [] = []
  goCs' ns (c:cs')
   = let c'  = allvarsC ns c
         ns' = Set.union ns (snd $ annotOfContext c')
     in  c' : goCs' ns' cs'

-- | Compute set of value variables of context
-- Because the context isn't a thing on its own, it needs the set of variables
-- used in the rest of the query.
allvarsC :: Eq n => Set.Set (Name n) -> Context a n -> Context (a, Set.Set (Name n)) n
allvarsC ns c
 = case c of
    Windowed a w1 w2
     -> Windowed (a,ns) w1 w2
    Latest a w1
     -> Latest (a,ns) w1

    Let a n x
     -> let x'  = allvarsX x
            ns' = Set.unions
                [ ns, sgl n, annX x' ]
        in  Let (a, ns') n x'

    LetFold a f
     -> let z'  = allvarsX (foldInit f)
            k'  = allvarsX (foldWork f)
            ns' = Set.unions
                [ ns, sgl (foldBind f)
                , annX z', annX k' ]
            f'  = f { foldInit = z', foldWork = k' }
        in  LetFold (a,ns') f'

    GroupBy a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  GroupBy (a,ns') x'

    GroupFold a nk nv x
     -> let x'  = allvarsX x
            ns' = Set.unions
                [ ns, sgl nk, sgl nv, annX x' ]
        in  GroupFold (a,ns') nk nv x'

    Distinct a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  Distinct (a,ns') x'

    Filter a x
     -> let x'  = allvarsX x
            ns' = Set.union ns (annX x')
        in  Filter (a,ns') x'

 where
  annX = snd . annotOfExp
  sgl  = Set.singleton


-- | Compute set of value variables of expression
allvarsX :: Eq n => Exp a n -> Exp (a, Set.Set (Name n)) n
allvarsX x
 = case x of
    Var a n
     -> Var (a, sgl n) n
    Nested a q
     -> let q' = allvarsQ q
        in  Nested (a, snd $ annotOfQuery q') q'
    App a p q
     -> let p' = allvarsX p
            q' = allvarsX q
        in  App (a, Set.union (annX p') (annX q')) p' q'
    Prim a p
     -> Prim (a, Set.empty) p
    Case a s ps
     -> let s'        = allvarsX s
            (ps',ns') = goPatXs ps
        in  Case (a, Set.union (annX s') ns') s' ps'
 where
  annX = snd . annotOfExp
  sgl  = Set.singleton

  goPatXs []
   = ([], Set.empty)
  goPatXs ((p,xx):ps)
   = let (ps',ns') = goPatXs  ps
         (p',np')  = goPat    p
         xx'       = allvarsX xx
         ns''      = Set.unions [ns', np', annX xx']
     in  ((p',xx') : ps', ns'')

  goPat p
   = case p of
      PatCon c ps
       -> let (ps',ns') = goPats ps
          in  (PatCon c ps', ns')
      PatDefault
       -> (PatDefault, Set.empty)
      PatVariable n
       -> (PatVariable n, sgl n)

  goPats []
   = ([], Set.empty)
  goPats (p:ps)
   = let (p',n')   = goPat p
         (ps',ns') = goPats ps
     in (p' : ps', n' `Set.union` ns')

