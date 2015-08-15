{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Transform.Base (
    Transform(..)
  , idTransform
  , transformQ
  , transformC
  , transformX
  ) where

import Icicle.Source.Query

import P

data Transform m c a n
 = Transform
 { transformExp     :: c
                    -> Exp a n
                    -> m (c, Exp a n)
 , transformContext :: c
                    -> Context a n
                    -> m (c, Context a n)
 , transformState   :: c
 }

idTransform :: Monad m => Transform m () a n
idTransform
 = Transform ret ret ()
 where
  ret s x = return (s,x)


transformQ
    :: (Monad m)
    => Transform m c a n
    -> Query a n
    -> m (Query a n)
transformQ t (Query ctxs xx)
 = do   (s',cs')  <- goCS (transformState t) ctxs
        xx' <- transformX (t { transformState = s' }) xx
        return $ Query cs' xx'
 where
  goCS s []
   = return (s, [])
  goCS s (c:cs)
   = do (s',c') <- transformC (t { transformState = s }) c
        (s'',cs') <- goCS s' cs
        return (s'', c' : cs')

transformC
    :: (Monad m)
    => Transform m c a n
    -> Context a n
    -> m (c, Context a n)
transformC t cc
 = do (s,cc') <- transformContext t (transformState t) cc
      let goX = transformX (t { transformState = s })
      case cc' of
       Windowed{} -> return (s,cc')
       Latest{} -> return (s,cc')
       GroupBy a x
        -> do x' <- goX x
              return (s, GroupBy a x')
       Distinct a x
        -> do x' <- goX x
              return (s, Distinct a x')
       Filter a x
        -> do x' <- goX x
              return (s, Filter a x')
       LetFold a f
        -> do  i' <- goX $ foldInit f
               w' <- goX $ foldWork f
               return (s, LetFold a (f { foldInit = i', foldWork = w' }))
       Let a n x
        -> do  x' <- goX x
               return (s, Let a n x')


transformX
    :: (Monad m)
    => Transform m c a n
    -> Exp a n
    -> m (Exp a n)
transformX t xx
 = do (s',xx') <- transformExp t (transformState t) xx
      let t' = t { transformState = s' }
      let goQ = transformQ t'
      let goX = transformX t'

      case xx' of
       Var{} -> return xx'
       Nested a q
        -> do q' <- goQ q
              return $ Nested a q'
       App a p q
        -> do p' <- goX p
              q' <- goX q
              return $ App a p' q'
       Prim{}
        -> return xx'


