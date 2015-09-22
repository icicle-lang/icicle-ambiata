{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.Transform.Desugar where

import           Icicle.Common.Base
import           Icicle.Common.Fresh
import           Icicle.Source.Query

import           P

import           Prelude             (error)


desugarQT :: (Eq n) => QueryTop a n -> Fresh n (QueryTop a n)
desugarQT qt
  = do qq' <- desugarQ (query qt)
       return $ qt { query = qq' }

desugarQ :: (Eq n) => Query a n -> Fresh n (Query a n)
desugarQ qq
  = do cs <- mapM desugarC (contexts qq)
       f  <- desugarX (final qq)
       return $ Query cs f

desugarC :: (Eq n) => Context a n -> Fresh n (Context a n)
desugarC cc
 = case cc of
    GroupBy  a   x -> GroupBy  a   <$> desugarX x
    Distinct a   x -> Distinct a   <$> desugarX x
    Filter   a   x -> Filter   a   <$> desugarX x
    Let      a n x -> Let      a n <$> desugarX x
    LetFold  a   f -> LetFold  a   <$> desugarF f
    _              -> return cc

desugarF :: (Eq n) => Fold (Query a n) a n -> Fresh n (Fold (Query a n) a n)
desugarF ff
  = do fi' <- desugarX (foldInit ff)
       fw' <- desugarX (foldWork ff)
       return $ ff { foldInit = fi', foldWork = fw'}

desugarX :: (Eq n) => Exp a n -> Fresh n (Exp a n)
desugarX xx
 = case xx of
    Nested a q
     -> do q' <- desugarQ q
           return $ Nested a q'

    Case a scrut patalts
     -> do let pats  = fmap fst patalts
           let ty    = foldl' (flip addToTy) TyAny pats
           tree     <- casesForTy a scrut ty
           return $ treeToCase a patalts tree

    App a x1 x2
      -> do x1' <- desugarX x1
            x2' <- desugarX x2
            return $ App a x1' x2'

    Var _ _
      -> return xx
    Prim _ _
     -> return xx

--------------------------------------------------------------------------------

-- * Case Flattening

-- | The partial "type" of patterns, up to where they are matched.
--   The type of the scrutinee is strictly more specific than this, but we don't
--   want to generate too many cases.
--
data Ty
 = TyTup Ty Ty
 | TyOpt Ty
 | TyBool
 | TyAny
 deriving (Show)


addToTy :: Pattern n -> Ty -> Ty
addToTy (PatCon ConTuple [p1, p2]) (TyTup t1 t2) = TyTup (addToTy p1 t1) (addToTy p2 t2)
addToTy (PatCon ConTuple [p1, p2]) TyAny         = TyTup (addToTy p1 TyAny) (addToTy p2 TyAny)
addToTy (PatCon ConSome  [p])      (TyOpt t)     = TyOpt (addToTy p t)
addToTy (PatCon ConSome  [p])      TyAny         = TyOpt (addToTy p TyAny)
addToTy (PatCon ConNone  [])       ty@(TyOpt _)  = ty
addToTy (PatCon ConNone  [])       TyAny         = TyOpt TyAny
addToTy (PatCon ConTrue  [])       ty@TyBool     = ty
addToTy (PatCon ConTrue  [])       TyAny         = TyBool
addToTy (PatCon ConFalse [])       ty@TyBool     = ty
addToTy (PatCon ConFalse [])       TyAny         = TyBool
addToTy _                          ty            = ty


casesForTy
  :: a -> Exp' (Query a n) a n -> Ty -> Fresh n (Tree a n (Pattern n))
casesForTy ann scrut ty
 = case ty of

    -- Booleans just have True/False
    TyBool
     -> return
      $ TCase scrut [ (PatCon ConTrue  [], Done (PatCon ConTrue  []))
                    , (PatCon ConFalse [], Done (PatCon ConFalse [])) ]

    -- Tuples treat arguments as nested cases
    TyTup a b
     -> do args     <- freshes 2
           let pat'  = PatCon ConTuple (fmap PatVariable args)
           let vars  = fmap (Var ann) args
           bd       <- liftM (fmap (PatCon ConTuple) . sequence)
                     $ zipWithM (casesForTy ann) vars [a,b]
           return $ TCase scrut [ (pat', bd) ]

    -- Options need a case for None, and nested cases for Some arguments
    TyOpt a
     -> do args     <- freshes 1
           let pat'  = PatCon ConSome (fmap PatVariable args)
           let vars  = fmap (Var ann) args
           bd       <- liftM (fmap (PatCon ConSome) . sequence)
                     $ zipWithM (casesForTy ann) vars [a]
           return $ TCase scrut [ (pat', bd)
                               , (PatCon ConNone [], Done (PatCon ConNone [])) ]

    -- If we don't know the type of this pattern, use a fresh variable
    -- Use TLet to avoid generating variable patterns, since Core can't handle them.
    TyAny
     -> do var <- fresh
           return $ TLet var scrut (Done (PatVariable var) )


-- | A nested case AST. We generate this from the patterns and convert it into
--   a case statement.
--
data Tree a n x
 = Done  x                         -- ^ just use the pattern/alternative/thing.
 | TCase (Exp' (Query a n) a n)    -- ^ do a case statement
         [(Pattern n, Tree a n x)]
 | TLet  (Name n)                  -- ^ insert a let because we cannot generate pattern variables.
         (Exp' (Query a n) a n)
         (Tree a n x)
 deriving (Functor, Show)

instance Monad (Tree a n) where
  return  = Done
  a >>= b = joinT (fmap b a)
   where
    joinT (Done x)     = x
    joinT (TCase n ls) = TCase n (fmap (fmap joinT) ls)
    joinT (TLet n x t) = TLet  n x (joinT t)


treeToCase
  :: (Pretty n, Show n, Show a, Eq n)
  => a
  -> [(Pattern n, Exp' (Query a n) a n)]
  -> Tree a n (Pattern n)
  -> Exp' (Query a n) a n
treeToCase ann patalts
 = simpCaseAlts . simpLets . caseStmtsFor . fmap (getAltBody patalts)
  where
   -- Convert tree structure to AST
   caseStmtsFor (Done x)
    = x
   caseStmtsFor (TCase scrut alts)
    = Case ann scrut (fmap (fmap caseStmtsFor) alts)
   caseStmtsFor (TLet n x e)
    = Nested ann (Query [Let ann n x] (caseStmtsFor e))

   -- Look up the alternative for this pattern
   getAltBody ((px, x) : xs) p
    = case matcher p px of
       Nothing -> getAltBody xs p
       Just [] -> x
       Just s  -> Nested ann (Query (fmap generateLet s) x)

   generateLet (n ,p)
     = Let ann n (patternToExp p)

   patternToExp (PatCon c as)
     = foldl (App ann) (Prim ann (PrimCon c)) (fmap patternToExp as)
   patternToExp (PatVariable v)
     = Var ann v
   patternToExp (PatDefault)
     = error "desugar: impossible! case flattening could not have generated default patterns!"

   -- "Unify" the generated pattern and a user-supplied pattern.
   -- Return a list of substitutions if success. This is necessary in case the
   -- generated patterns are more specific than the user's pattern, e.g.
   -- if we have `None` and the user just supplies a variable.
   --
   -- Precondition: matcher p q assumes p is more specific than q
   -- Precondition: matcher p q assumes p contains no PatDefault
   matcher (PatCon c as) (PatCon c' bs)
    = do guard (c == c')
         substs <- zipWithM matcher as bs
         return (concat substs)
   matcher p (PatVariable x)
    = return [(x, p)]
   matcher _ (PatDefault)
    = return []
   matcher _ _
    = Nothing


-- | Simplify nested bindings from variables to variables, e.g. `let x = y ~>..`
--   This recurses into nested queries so it's not a traditional beta-reduction.
--
simpLets :: (Eq n) => Exp' (Query a n) a n -> Exp' (Query a n) a n
simpLets xx
 = case xx of
    Nested _ q
     -> go q

    Case a e ps
     -> Case a e $ fmap (fmap simpLets) ps

    _ -> xx

 where
  go qq
   = case qq of
       Query [] bd
         -> simpLets bd
       Query (Let _ x (Var _ y) : cs) bd
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
  varsIn (PatDefault)    = []

  -- substC x y cc| trace ("SIMP: SUBSTC: \n" <> (show $ pretty x) <> " " <> (show $ pretty y) <> " " <> (show $ pretty cc)) False = undefined
  substC _ _ []
   = (True, [])
  substC x y (cc:rest)
   = let (f, rest') = substC x y rest
     in  case cc of
       Let a n e
        | n /= x
        -> (f, Let a n (substX x y e) : rest')
        | n == x
        -> (False, Let a n (substX x y e) : rest)
       LetFold a (Fold n init work ty)
        | n /= x
        -> (f, LetFold a (Fold n (substX x y init) (substX x y work) ty):rest')
        | n == x
        -> (False, LetFold a (Fold n (substX x y init) work ty) : rest)
       GroupBy a e
        -> (f, GroupBy a (substX x y e) : rest')
       Distinct a e
        -> (f, Distinct a (substX x y e) : rest')
       Filter a e
        -> (f, Filter a (substX x y e) : rest')

       Windowed {} -> (f, cc : rest')
       Latest {}   -> (f, cc :rest')

  substQ x y qq
   = let (f, ctxs) = substC x y (contexts qq)
      in qq { contexts = ctxs
            , final    = if f then substX x y (final qq) else final qq }


freshes :: Monad m => Int -> FreshT n m [Name n]
freshes n = replicateM n fresh
