{-# LANGUAGE NoImplicitPrelude #-}

-- | * Simplify dumb roundabout bindings.
--     e.g. `let x = y in body` or `case x | y -> body`
--
module Icicle.Source.Transform.Simp.Dumb
  ( dumbSimpTransform
  , simpDumbQT
  , simpDumbQ
  , simpDumbX
  ) where

import           Icicle.Source.Query
import           Icicle.Source.Transform.Base

import           Data.Functor.Identity

import           P


dumbSimpTransform
  :: (Eq n)
  => Transform Identity () a n
dumbSimpTransform
 = Transform
 { transformExp     = tranx
 , transformPat     = tranp
 , transformContext = tranc
 , transformState   = ()
 }
 where
  tranx _ x
   = return ((), simpDumbX x)
  tranp _ p
   = return ((), p)
  tranc _ c
   = return ((), simpDumbC c)


simpDumbQT :: Eq n => QueryTop a n -> QueryTop a n
simpDumbQT qt
 = runIdentity $ simplifyNestedQT <$> transformQT dumbSimpTransform qt

simpDumbQ :: Eq n => Query a n -> Query a n
simpDumbQ qq
 = runIdentity $ transformQ dumbSimpTransform qq

simpDumbC :: Eq n => Context a n -> Context a n
simpDumbC cc
 = case cc of
    GroupBy   a   x   -> GroupBy   a     $ simpDumbX x
    Distinct  a   x   -> Distinct  a     $ simpDumbX x
    Filter    a   x   -> Filter    a     $ simpDumbX x
    Let       a n x   -> Let       a n   $ simpDumbX x
    LetFold   a   f   -> LetFold   a     $ simpDumbF f
    GroupFold a k v x -> GroupFold a k v $ simpDumbX x
    Windowed{}        -> cc
    Latest{}          -> cc

simpDumbF :: Eq n => Fold (Query a n) a n -> Fold (Query a n) a n
simpDumbF ff
 = ff { foldInit = simpDumbX (foldInit ff)
      , foldWork = simpDumbX (foldWork ff) }

simpDumbX :: Eq n => Exp' (Query a n) a n -> Exp' (Query a n) a n
simpDumbX = simpDumbCases . simpDumbLets

-- | Simplify cases with a single default/variable pattern.
--
simpDumbCases
 :: Exp' (Query a n) a n -> Exp' (Query a n) a n
simpDumbCases xx
 = case xx of
    Case _ _ [(PatDefault, x)]
     -> simpX x

    Case a e [(PatVariable n, x)]
     -> let q = Query [Let a (PatVariable n) (simpX e)] (simpX x)
        in  Nested a q

    Case a e ps
     -> Case a (simpX e) (fmap (fmap simpX) ps)

    Nested a q
     -> Nested a (simpQ q)

    App a x y
     -> App a (simpX x) (simpX y)

    Var{}  -> xx
    Prim{} -> xx

 where
  simpX
   = simpDumbCases

  simpQ qq
   = qq { contexts = fmap simpC (contexts qq)
        , final    = simpX (final qq) }

  simpC cc
   = case cc of
      GroupBy a x
       -> GroupBy a (simpX x)
      Distinct a x
       -> Distinct a (simpX x)
      Filter a x
       -> Filter a (simpX x)
      LetFold a (Fold b i w t)
       -> LetFold a (Fold b (simpX i) (simpX w) t)
      Let a n x
       -> Let a n (simpX x)
      GroupFold a n1 n2 x
       -> GroupFold a n1 n2 (simpX x)
      Windowed{} -> cc
      Latest{}   -> cc

-- | Simplify nested bindings from variables to variables, e.g. `let x = y ~>..`
--   This recurses into nested queries so it's not a traditional beta-reduction.
--
simpDumbLets :: (Eq n) => Exp' (Query a n) a n -> Exp' (Query a n) a n
simpDumbLets xx
 = case xx of
    Nested _ q
     -> go q

    Case a e ps
     -> Case a e $ fmap (fmap simpDumbLets) ps

    _ -> xx

 where
  go qq
   = case qq of
       Query [] bd
         -> simpDumbLets bd
       Query (Let _ (PatVariable x) (Var _ y) : cs) bd
         -> go (Query cs (substX x y bd))
       _ -> xx

  substX x y bd
   = case bd of
      Var a n
        | n == x
        -> Var a y
      Nested a q
        -> Nested a $ substQ x y q
      App a e1 e2
        -> App a (substX x y e1) (substX x y e2)
      Case a e pats
        -> Case a (substX x y e) (fmap (substA x y) pats)
      _ -> bd

  substA x y (pat, e)
   | x `elem` varsIn pat = (pat, e)
   | otherwise           = (pat, substX x y e)

  varsIn (PatCon _ as)   = concatMap varsIn as
  varsIn (PatVariable v) = [ v ]
  varsIn (PatLit _ _)      = []
  varsIn (PatDefault)    = []

  substC _ _ []
   = (True, [])
  substC x y (cc:rest)
   = let (f, rest') = substC x y rest
     in  case cc of
       Let a pat e
        | x `elem` varsIn pat
        -> (False, Let a pat (substX x y e) : rest)
        | otherwise
        -> (f, Let a pat (substX x y e) : rest')
       LetFold a (Fold pat init work ty)
        | x `elem` varsIn pat
        -> (f, LetFold a (Fold pat (substX x y init) (substX x y work) ty):rest')
        | otherwise
        -> (False, LetFold a (Fold pat (substX x y init) work ty) : rest)
       GroupBy a e
        -> (f, GroupBy a (substX x y e) : rest')
       Distinct a e
        -> (f, Distinct a (substX x y e) : rest')
       Filter a e
        -> (f, Filter a (substX x y e) : rest')
       GroupFold a n1 n2 e
        -> (f, GroupFold a n1 n2 (substX x y e) : rest')

       Windowed {} -> (f, cc : rest')
       Latest {}   -> (f, cc : rest')

  substQ x y qq
   = let (f, ctxs) = substC x y (contexts qq)
      in qq { contexts = ctxs
            , final    = if f then substX x y (final qq) else final qq }
