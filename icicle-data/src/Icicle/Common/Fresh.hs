-- | Generating Fresh names
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ExistentialQuantification #-}
module Icicle.Common.Fresh (
      FreshT   (..)
    , Fresh
    , runFresh
    , runFreshIdentity
    , NameState
    , mkNameState
    , counterNameState
    , counterPrefixNameState
    , fresh
    , freshBase
    , freshPrefix
    , freshPrefixBase
    ) where

import              Icicle.Common.Base

import              P

import              Control.Monad.Trans.Class
import              Data.Functor.Identity
import              Data.Hashable

newtype FreshT n m a
 = FreshT
 { runFreshT :: NameState n -> m (NameState n, a) }

type Fresh n = FreshT n Identity

runFresh :: Fresh n a -> NameState n -> (NameState n, a)
runFresh f ns
 = runIdentity
 $ runFreshT f ns

-- argh
runFreshIdentity :: Monad m => Fresh n x -> FreshT n m x
runFreshIdentity f
 = FreshT
 $ \ns -> return $ runFresh f ns

--------------------------------------------------------------------------------

data NameState n
 = forall s. NameState (s -> (NameBase n, s)) s

-- we don't care about sequencing the name state, just need this for
-- functions of type FreshT n m a. The `a` is the only thing we care
-- about.
instance NFData (NameState n) where rnf _ = ()

mkNameState :: (s -> (NameBase n, s)) -> s -> NameState n
mkNameState = NameState

counterNameState :: (Int -> NameBase n) -> Int -> NameState n
counterNameState f i
 = mkNameState (\i' -> (f i', i'+1)) i

counterPrefixNameState :: (Int -> n) -> n -> NameState n
counterPrefixNameState print prefix
 = counterNameState (NameMod prefix . NameBase . print) 0

fresh :: (Hashable n, Monad m) => FreshT n m (Name n)
fresh = freshBase >>= return . nameOf

freshBase :: Monad m => FreshT n m (NameBase n)
freshBase
 = FreshT
 $ \(NameState step state)
 -> let (n, s) = step state
    in  return (NameState step s, n)

freshPrefix :: (Hashable n, Monad m) => n -> FreshT n m (Name n)
freshPrefix pre
 = freshPrefixBase (NameBase pre)

freshPrefixBase :: (Hashable n, Monad m) => NameBase n -> FreshT n m (Name n)
freshPrefixBase pre
 = do n <- freshBase
      return $ nameOf $ prefix pre n
 where
  prefix (NameBase a)   b = NameMod a b
  prefix (NameMod  a b) c = NameMod a (prefix b c)

--------------------------------------------------------------------------------

instance Monad m => Monad (FreshT n m) where
 (>>=) p q
  = FreshT
  $ \ns
  -> do (ns',a) <- runFreshT p ns
        runFreshT (q a) ns'

 return a = FreshT $ \ns -> return (ns, a)

instance Monad m => Functor (FreshT n m) where
 fmap f p
  = p >>= (return . f)

instance Monad m => Applicative (FreshT n m) where
 pure = return
 (<*>) f x
  = do f' <- f
       x' <- x
       return $ f' x'
 

instance MonadTrans (FreshT n) where
 lift m
  = FreshT
  $ \ns
  -> do v <- m
        return (ns, v)

