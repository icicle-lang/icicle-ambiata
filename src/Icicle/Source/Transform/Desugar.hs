{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Transform.Desugar
  ( DesugarError(..)
  , runDesugar
  , desugarQT
  , desugarQ
  , desugarFun
  ) where

import           Icicle.Common.Base
import           Icicle.Common.Fresh

import           Icicle.Source.Query
import           Icicle.Source.Transform.Simp
import           Icicle.Internal.Pretty

import           Data.Functor.Identity

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class

import           P


data DesugarError a n
 = DesugarErrorNoAlternative a (Pattern n) -- ^ we generated a pattern that cannot be matched
                                           --   with any alternative.
 | DesugarErrorImpossible                  -- ^ just impossible, the world has ended.
 | DesugarOverlappingPattern a (Pattern n) -- ^ duh
 | DesugarIllTypedPatterns   a [Pattern n] -- ^ patterns use constructors from different types
 deriving (Eq, Show)

instance (Pretty a, Pretty n) => Pretty (DesugarError a n) where
  pretty (DesugarErrorNoAlternative a n) = "Missing alternative:" <+> pretty n <+> "at" <+> pretty a
  pretty (DesugarErrorImpossible)        = "Impossible desugar error."
  pretty (DesugarOverlappingPattern a x) = "Overlapping pattern:" <+> pretty x <+> "at" <+> pretty a
  pretty (DesugarIllTypedPatterns a xs)  = "Illtyped patterns:"   <+> align (vcat (pretty <$> xs)) <> line <> "at" <+> pretty a

type DesugarM a n x = FreshT n (EitherT (DesugarError a n) Identity) x

runDesugar :: NameState n -> DesugarM a n x -> Either (DesugarError a n) x
runDesugar n m = runIdentity . runEitherT . bimapEitherT id snd $ runFreshT m n

desugarFun
  :: (Eq n)
  => Function a n
  -> DesugarM a n (Function a n)
desugarFun f
  = do b' <- desugarQ (body f)
       return $ f { body = b'}

desugarQT
  :: (Eq n)
  => QueryTop a n
  -> DesugarM a n (QueryTop a n)
desugarQT qt
  = do qq' <- desugarQ (query qt)
       return $ qt { query = qq' }

desugarQ
  :: (Eq n)
  => Query a n
  -> DesugarM a n (Query a n)
desugarQ qq
  = do cs <- mapM desugarC (contexts qq)
       f  <- desugarX (final qq)
       return $ Query cs f

desugarC
  :: (Eq n)
  => Context a n
  -> DesugarM a n (Context a n)
desugarC cc
 = case cc of
    GroupBy   a   x   -> GroupBy   a     <$> desugarX x
    Distinct  a   x   -> Distinct  a     <$> desugarX x
    Filter    a   x   -> Filter    a     <$> desugarX x
    Let       a n x   -> Let       a n   <$> desugarX x
    LetFold   a   f   -> LetFold   a     <$> desugarF f
    GroupFold a k v x -> GroupFold a k v <$> desugarX x
    Windowed{}        -> return cc
    Latest{}          -> return cc

desugarF
  :: (Eq n)
  => Fold (Query a n) a n
  -> DesugarM a n (Fold (Query a n) a n)
desugarF ff
  = do fi' <- desugarX (foldInit ff)
       fw' <- desugarX (foldWork ff)
       return $ ff { foldInit = fi', foldWork = fw'}

desugarX
  :: (Eq n)
  => Exp a n
  -> DesugarM a n (Exp a n)
desugarX xx
 = case xx of
    Nested a q
     -> do q' <- desugarQ q
           return $ Nested a q'

    Case a scrut patalts
     -> do let pats  = fmap fst patalts
           ty       <- foldM (flip $ addToTy $ DesugarIllTypedPatterns a pats) TyAny pats

           scrut'   <- desugarX scrut
           patalts' <- mapM (mapM desugarX) patalts

           tree     <- casesForTy a scrut' ty
           checkOverlapping a pats (toList tree)

           treeToCase a patalts' tree

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
 | TySum Ty Ty
 | TyBool
 -- Literals such as numbers and argument-less enums:
 -- where we can, instead of doing a real case, check against "x == Con".
 -- This doesn't work for Bool because this desugaring uses Bool
 | TyLit [Constructor]
 | TyAny
 deriving (Show)


addToTy :: DesugarError a n -> Pattern n -> Ty -> DesugarM a n Ty
addToTy err (PatCon con pats) ty
 = case con of
    ConTuple
     | [p1, p2]         <- pats
     , TyTup t1 t2      <- ty
     -> TyTup <$> go p1 t1    <*> go p2 t2
     | [p1, p2]         <- pats
     , TyAny            <- ty
     -> TyTup <$> go p1 TyAny <*> go p2 TyAny
     | otherwise
     -> lift $ left err

    ConSome
     | [p]              <- pats
     , TyOpt t          <- ty
     -> TyOpt <$> go p t
     | [p]              <- pats
     , TyAny            <- ty
     -> TyOpt <$> go p TyAny
     | otherwise
     -> lift $ left err

    ConNone
     | []               <- pats
     , TyOpt _          <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return $ TyOpt TyAny
     | otherwise
     -> lift $ left err

    ConTrue
     | []               <- pats
     , TyBool           <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return TyBool
     | otherwise
     -> lift $ left err

    ConFalse
     | []               <- pats
     , TyBool           <- ty
     -> return ty
     | []               <- pats
     , TyAny            <- ty
     -> return TyBool
     | otherwise
     -> lift $ left err

    ConLeft
     | [p]              <- pats
     , TySum t1 t2      <- ty
     -> TySum <$> go p t1    <*> return t2
     | [p]              <- pats
     , TyAny            <- ty
     -> TySum <$> go p TyAny <*> return TyAny
     | otherwise
     -> lift $ left err

    ConRight
     | [p]              <- pats
     , TySum t1 t2      <- ty
     -> TySum <$> return t1    <*> go p t2
     | [p]              <- pats
     , TyAny            <- ty
     -> TySum <$> return TyAny <*> go p TyAny
     | otherwise
     -> lift $ left err

    ConError _
     | []               <- pats
     , TyLit cs         <- ty
     -> return $ TyLit (cs <> [con])
     | []               <- pats
     , TyAny            <- ty
     -> return $ TyLit [con]
     | otherwise
     -> lift $ left err
 where
  go = addToTy err

addToTy _ PatDefault      ty    = return ty
addToTy _ (PatVariable _) ty    = return ty


casesForTy
  :: a
  -> Exp' (Query a n) a n
  -> Ty
  -> DesugarM a n (Tree a n (Pattern n))
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
           bd       <- subtree ConTuple vars [a, b]
           return $ TCase scrut [ (pat', bd) ]

    -- Options need a case for None, and nested cases for Some arguments
    TyOpt a
     -> do args     <- freshes 1
           let pat'  = PatCon ConSome (fmap PatVariable args)
           let vars  = fmap (Var ann) args
           bd       <- subtree ConSome vars [a]
           return $ TCase scrut [ (pat', bd)
                               , (PatCon ConNone [], Done (PatCon ConNone [])) ]

    -- Sums need nested cases for both
    TySum a b
     -> do aleft     <- freshes 1
           let pleft  = PatCon ConLeft  (fmap PatVariable aleft)
           aright    <- freshes 1
           let pright = PatCon ConRight (fmap PatVariable aright)
           let vars   = fmap (Var ann)
           bl <- subtree ConLeft  (vars aleft)  [a]
           br <- subtree ConRight (vars aright) [b]
           return $ TCase scrut [ (pleft,  bl)
                                , (pright, br) ]

    TyLit cs
     -> do var <- fresh
           return $ TLet var scrut
                  $ TLits   (Var ann var)
                            (fmap (\c -> (c, Done $ PatCon c [])) cs)
                            (Done $ PatVariable var)

    -- If we don't know the type of this pattern, use a fresh variable
    -- Use TLet to avoid generating variable patterns, since Core can't handle them.
    TyAny
     -> do var <- fresh
           return $ TLet var scrut (Done (PatVariable var) )

 where
  subtree con vars tys
   = liftM (fmap (PatCon con) . sequence)
   $ zipWithM (casesForTy ann) vars tys

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
 | TLits (Exp' (Query a n) a n)    -- ^ special case for literals
         [(Constructor, Tree a n x)] (Tree a n x)
 deriving (Functor, Foldable, Traversable, Show)


instance Monad (Tree a n) where
  return  = Done
  a >>= b = joinT (fmap b a)
   where
    joinT (Done x)     = x
    joinT (TCase n ls) = TCase n (fmap (fmap joinT) ls)
    joinT (TLet n x t) = TLet  n x (joinT t)
    joinT (TLits  s cs d)
                       = TLits s (fmap (fmap joinT) cs) (joinT d)


treeToCase
  :: (Eq n)
  => a
  -> [(Pattern n, Exp' (Query a n) a n)]
  -> Tree a n (Pattern n)
  -> DesugarM a n (Exp' (Query a n) a n)
treeToCase ann patalts tree
 = lift . fmap (simpDumbX . caseStmtsFor) . sequence
 $ fmap (getAltBody patalts) tree
  where
   -- Convert tree structure to AST
   caseStmtsFor (Done x)
    = x
   caseStmtsFor (TCase scrut alts)
    = Case ann scrut (fmap (fmap caseStmtsFor) alts)
   caseStmtsFor (TLet n x e)
    = Nested ann (Query [Let ann n x] (caseStmtsFor e))

   caseStmtsFor (TLits scrut ((c,x):cs) d)
    = let eq = Prim ann $ Op $ Relation Eq
          c' = Prim ann $ PrimCon c
          sc = App ann (App ann eq scrut) c'
      in Case ann sc
            [ ( PatCon ConTrue  [], caseStmtsFor x )
            , ( PatCon ConFalse [], caseStmtsFor $ TLits scrut cs d ) ]
   caseStmtsFor (TLits _ [] d)
    = caseStmtsFor d

   -- Look up the alternative for this pattern
   getAltBody ((px, x) : xs) p
    = case matcher p px of
       Nothing
        -> getAltBody xs p
       Just []
        -> right x
       Just s
        -> do s' <- mapM generateLet s
              right $ Nested ann (Query s' x)
   getAltBody _ p
    = left $ DesugarErrorNoAlternative ann p

   generateLet (n ,p)
    = Let ann n <$> patternToExp p

   patternToExp (PatCon c as)
    = do xs <- mapM patternToExp as
         right $ foldl (App ann) (Prim ann (PrimCon c)) xs
   patternToExp (PatVariable v)
    = right $ Var ann v
   patternToExp PatDefault
    = left DesugarErrorImpossible -- we never generate default patterns.

-- "Unify" the generated pattern and a user-supplied pattern.
-- Return a list of substitutions if success. This is necessary in case the
-- generated patterns are more specific than the user's pattern, e.g.
-- if we have `None` and the user just supplies a variable.
--
-- Precondition: matcher p q assumes p is more specific than q
-- Precondition: matcher p q assumes p contains no PatDefault
matcher :: Pattern t -> Pattern t -> Maybe [(Name t, Pattern t)]
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


checkOverlapping
  :: a -> [Pattern n] -> [Pattern n] -> DesugarM a n ()
checkOverlapping ann userpats genpats
  = foldM_ (\p -> lift . go p) genpats userpats
  where
   go gps up
    = let gps' = filter (\gp -> isNothing $ matcher gp up ) gps
      in  if length gps' < length gps
          then return gps'
          else left (DesugarOverlappingPattern ann up)


freshes :: Monad m => Int -> FreshT n m [Name n]
freshes n = replicateM n fresh
