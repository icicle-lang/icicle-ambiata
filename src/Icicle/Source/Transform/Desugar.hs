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


desugarQT :: QueryTop a n -> Fresh n (QueryTop a n)
desugarQT qt
  = do qq' <- desugarQ (query qt)
       return $ qt { query = qq' }

desugarQ :: Query a n -> Fresh n (Query a n)
desugarQ qq
  = do cs <- mapM desugarC (contexts qq)
       f  <- desugarX (final qq)
       return $ Query cs f

desugarC :: Context a n -> Fresh n (Context a n)
desugarC cc
 = case cc of
    GroupBy  a   x -> GroupBy  a   <$> desugarX x
    Distinct a   x -> Distinct a   <$> desugarX x
    Filter   a   x -> Filter   a   <$> desugarX x
    Let      a n x -> Let      a n <$> desugarX x
    LetFold  a   f -> LetFold  a   <$> desugarF f
    _              -> return cc

desugarF :: Fold (Query a n) a n -> Fresh n (Fold (Query a n) a n)
desugarF ff
  = do fi' <- desugarX (foldInit ff)
       fw' <- desugarX (foldWork ff)
       return $ ff { foldInit = fi', foldWork = fw'}

desugarX :: Exp a n -> Fresh n (Exp a n)
desugarX xx
 = case xx of

    Case a scrut patalts
     -> do let pats  = fmap fst patalts
           let ty    = foldl' (flip addToTy) TyAny pats
           tree     <- casesForTy a scrut ty
           return $ treeToCase a patalts tree

    _ -> return xx

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
 = Done  x                         -- ^ just use pattern/alternative/thing.
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
  :: a
  -> [(Pattern n, Exp' (Query a n) a n)]
  -> Tree a n (Pattern n)
  -> Exp' (Query a n) a n
treeToCase ann patalts
 = caseStmtsFor . fmap (getAltBody patalts)
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


freshes :: Monad m => Int -> FreshT n m [Name n]
freshes n = replicateM n fresh
