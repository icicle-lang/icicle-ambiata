{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.SubstX (
    substX
  , substQ
  ) where

import Icicle.Source.Query
import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Hashable (Hashable)


substX :: (Hashable n, Eq n)
       => (Map.Map (Name n) (Exp a n))
       -> Exp a n
       -> Fresh n (Exp a n)
substX ms x
 = reannotX fst
 <$> substX' (fmap allvarsX ms) (allvarsX x)

substQ :: (Hashable n, Eq n)
       => (Map.Map (Name n) (Exp a n))
       -> Query a n
       -> Fresh n (Query a n)
substQ ms q
 = reannotQ fst
 <$> substQ' (fmap allvarsX ms) (allvarsQ q)

--------------
-- Where the actual magic happens
-- These could be exposed, except the variable set is not updated,
-- so it will be wrong after applying.
-- This is not a problem since the functions above throw it away.

substX'
    :: (Hashable n, Eq n)
    => (Map.Map (Name n) (Exp (a, Set.Set (Name n)) n))
    -> Exp (a, Set.Set (Name n)) n
    -> Fresh n (Exp (a, Set.Set (Name n)) n)

substX' s x
 = case x of
    Var _ n
     | Just x' <- Map.lookup n s
     -> return x'
     | otherwise
     -> return x
    Nested a q
     -> Nested a <$> substQ' s q
    App a p q
     -> App a <$> substX' s p <*> substX' s q
    Prim{}
     -> return x
    Case a scrut pats
     -> Case a <$> substX' s scrut <*> mapM goPat pats

 where
  goPat (p,alt)
   = do (s',p') <- rempatbind s p
        alt'    <- substX' s' alt
        return (p',alt')

  rempatbinds m [] ret
   = return (m, reverse ret)
  rempatbinds m (p:ps) ret
   = do (m',p') <- rempatbind m p
        rempatbinds m' ps (p' : ret)

  rempatbind m p
   = case p of
      PatCon c ps
       -> do (m',ps') <- rempatbinds m ps []
             return (m', PatCon c ps')
      PatDefault
       -> return (m, p)
      PatVariable n
       -> do (m',n') <- freshIfNecessary m n (fst $ annotOfExp x)
             return (m', PatVariable n')


substQ' :: (Hashable n, Eq n)
        => (Map.Map (Name n) (Exp (a, Set.Set (Name n)) n))
        -> Query (a, Set.Set (Name n)) n
        -> Fresh n (Query (a, Set.Set (Name n)) n)

substQ' s (Query [] x)
 = Query [] <$> substX' s x

substQ' s (Query (c:rest_cs) rest_x)
 = case c of
    Let a p x
     -> do x'        <- substX' s x
           (s', p')  <- rempatbind s p
           rest s' (Let a p' x')

    LetFold a f
     -> do z'       <- substX' s  (foldInit f)
           (s', p') <- rempatbind s (foldBind f)
           k'       <- substX' s' (foldWork f)
           let f'    = f { foldBind = p', foldInit = z', foldWork = k' }
           rest s' (LetFold a f')
    Windowed{}
     -> rest s  c
    Latest{}
     -> rest s c
    GroupBy a x
     -> do x'       <- substX s x
           rest s (GroupBy a x')
    GroupFold a k v x
     -> do x'       <- substX s x
           (s', k') <- freshIfNecessary s  k (fst a)
           (s'',v') <- freshIfNecessary s' v (fst a)
           rest s'' (GroupFold a k' v' x')
    Distinct a x
     -> do x'       <- substX s x
           rest s (Distinct a x')
    Filter a x
     -> do x'       <- substX s x
           rest s (Filter a x')
 where
  ins cx' (Query cs' x') = Query (cx' : cs') x'
  q                      = Query rest_cs rest_x
  rest s' cx'            = ins cx' <$> substQ' s' q

  rempatbinds m [] ret
   = return (m, reverse ret)
  rempatbinds m (p:ps) ret
   = do (m',p') <- rempatbind m p
        rempatbinds m' ps (p' : ret)

  rempatbind m p
   = case p of
      PatCon con ps
       -> do (m',ps') <- rempatbinds m ps []
             return (m', PatCon con ps')
      PatDefault
       -> return (m, p)
      PatVariable n
       -> do (m',n') <- freshIfNecessary m n (fst $ annotOfExp rest_x)
             return (m', PatVariable n')

freshIfNecessary :: (Hashable n, Eq n)
      => (Map.Map (Name n) (Exp (a, Set.Set (Name n)) n))
      -> Name n
      -> a
      -> Fresh n (Map.Map (Name n) (Exp (a, Set.Set (Name n)) n), Name n)
freshIfNecessary m n a_fresh
 = case Map.lookup n m of
    Nothing
     -> if    Set.member n allvars
        then  freshen
        else  return (m,n)
    Just _
     -> return (Map.delete n m, n)
 where
  allvars
   = Set.unions $ Map.elems $ fmap (snd . annotOfExp) m

  freshen
   = do n' <- freshPrefixBase $ nameBase n
        let x = Var (a_fresh, Set.singleton n') n'
        return (Map.insert n x m, n')

