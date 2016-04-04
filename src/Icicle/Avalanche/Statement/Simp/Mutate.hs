{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Mutate where

import           Icicle.Common.Exp
import           Icicle.Common.Base
import qualified Icicle.Avalanche.Prim.Flat     as F
import           Icicle.Avalanche.Statement.Statement

import P

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Hashable


mutate
  :: (Hashable n, Eq n)
  => Statement a n F.Prim
  -> Statement a n F.Prim
mutate = mutateStmt Set.empty


mutateStmt
  :: (Hashable n, Eq n)
  => Set (Name n)
  -> Statement a n F.Prim -> Statement a n F.Prim
mutateStmt livevars ss
  = let go = mutateStmt livevars
    in case ss of
         If x s1 s2
           -> let lvs = livevars <> livevarsStmt s1 <> livevarsStmt s2
              in If (mutateExp lvs x)
                    (go s1)
                    (go s2)

         Let n x s
           -> let lvs = livevars <> livevarsStmt s
              in  Let n (mutateExp lvs x)
                        (go s)

         ForeachInts n x y s
           -> let lvs = livevars <> livevarsStmt s
              in  ForeachInts n (mutateExp livevars x)
                                (mutateExp lvs y)
                                (go s)

         ForeachFacts fs vt ft s
           -> ForeachFacts fs vt ft (go s)

         Block stmts
           -> let f (st:sts) = mutateStmt (livevars <> Set.unions (fmap livevarsStmt sts)) st
                              : fmap go sts
                  f []       = []
              in  Block $ f stmts

         InitAccumulator acc s
           -> let lvs  = livevars <> livevarsStmt s
                  acc' = acc { accInit = mutateExp lvs (accInit acc) }
              in  InitAccumulator acc' (go s)

         Read local acc t s
           -> Read local acc t (go s)

         -- If n shadows a binding in x, n is not live. Because after this line
         -- any reference to n will be the n bound here, not the n bound in x.
         Write n x
           -> let lvs = Set.delete n livevars
              in  Write n (mutateExp lvs x)

         Output n t xs
           -> Output n t (fmap (first (mutateExp livevars)) xs)

         KeepFactInHistory x
           -> KeepFactInHistory (mutateExp livevars x)

         LoadResumable{} -> ss
         SaveResumable{} -> ss


mutateExp :: (Hashable n, Eq n) => Set (Name n) -> Exp a n F.Prim -> Exp a n F.Prim
mutateExp livevars xx = case xx of
  XVar{}   -> xx
  XPrim{}  -> xx
  XValue{} -> xx
  XLam a n t x
   -> XLam a n t (mutateExp livevars x)
  XLet a n x1 x2
   -> XLet a n   (mutateExp livevars x1) (mutateExp livevars x2)
  XApp{}
   | (x, xs)                                            <- takeApps xx
   , XPrim ax (F.PrimArray (F.PrimArrayPutImmutable t)) <- x
   , [XVar _ arr,_,_]                                   <- xs
   , Set.notMember arr livevars
   -> let xs' = fmap (mutateExp livevars) xs
          x'  = XPrim ax (F.PrimArray (F.PrimArrayPutMutable t))
          axx = annotOfExp xx
      in makeApps axx x' xs'
   | otherwise -> xx


livevarsStmt :: (Eq n) => Statement a n p -> Set (Name n)
livevarsStmt ss = case ss of
  -- all live vars in x are live here.
  -- all live vars in both branches are live here.
  If x s1 s2
   -> livevarsExp x <> livevarsStmt s1 <> livevarsStmt s2

  -- n is not live at this point, because it is going to be bound.
  -- all live vars in both the binder and body are live here.
  Let _ x s
   -> livevarsExp x <> livevarsStmt s

  -- loop counter is live at the beginning and inside the loop.
  ForeachInts n _ end body
   -> Set.insert n (livevarsExp end <> livevarsStmt body)

  -- fact bind is live at the beginning and inside the loop.
  ForeachFacts bs _ _ body
   -> livevarsFactBinds bs <> livevarsStmt body

  -- something is live at the beginning of the block if it is live
  -- anywhere in the block.
  Block bs
   -> Set.unions $ fmap livevarsStmt bs

  -- when an accumulator is initialised, it is not yet live.
  -- but it is live inside the body statement.
  InitAccumulator _ body
   -> livevarsStmt body

  -- when an accumulator is read into a local binding, the local
  -- binding is not yet live, the accumulator is still alive at this line.
  Read _ acc _ body
   -> Set.insert acc (livevarsStmt body)

  -- when an accumulator is written to, it's not yet live.
  Write _ x
   -> livevarsExp x

  -- output an expression means all bindings in the output expressions are live.
  Output _ _ xs
   -> Set.unions $ fmap (livevarsExp . fst) xs

  -- not sure about these
  KeepFactInHistory x -> livevarsExp x
  LoadResumable n _   -> Set.singleton n
  SaveResumable n _   -> Set.singleton n


livevarsExp :: (Eq n) => Exp a n p -> Set (Name n)
livevarsExp xx = case xx of
  XVar _ n       -> Set.singleton n
  XPrim{}        -> Set.empty
  XValue{}       -> Set.empty
  XApp _ p q     -> livevarsExp p <> livevarsExp q
  XLam _ _ _ x   -> livevarsExp x
  XLet _ _ x1 x2 -> livevarsExp x1 <> livevarsExp x2


livevarsFactBinds :: (Eq n) => FactBinds n -> Set (Name n)
livevarsFactBinds b
  = Set.insert (factBindTime b)
  $ Set.insert (factBindId b)
  $ Set.fromList
  $ fmap fst (factBindValue b)
