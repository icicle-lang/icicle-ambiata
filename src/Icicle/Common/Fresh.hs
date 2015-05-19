-- | Generating Fresh names
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Icicle.Common.Fresh (
      FreshT   (..)
    , Fresh
    , runFresh
    , NameState
    , mkNameState
    , counterNameState
    , counterPrefixNameState
    , fresh
    ) where

import              Icicle.Common.Base
import              P

import              Control.Monad.Trans.Class
import              Data.Functor.Identity
import qualified    Data.Text as T

newtype FreshT n m a
 = FreshT
 { runFreshT :: NameState n -> m (NameState n, a) }

type Fresh n a = FreshT n Identity a

runFresh :: Fresh n a -> NameState n -> (NameState n, a)
runFresh f ns
 = runIdentity
 $ runFreshT f ns

data NameState n
 = forall s.
 NameState (s -> (Name n, s))
            s

mkNameState :: (s -> (Name n, s)) -> s -> NameState n
mkNameState = NameState

counterNameState :: (Int -> Name n) -> Int -> NameState n
counterNameState f i
 = mkNameState (\i' -> (f i', i'+1)) i

counterPrefixNameState :: T.Text -> NameState T.Text
counterPrefixNameState prefix
 = counterNameState (\i -> NameMod prefix $ Name $ T.pack $ show i) 0

fresh :: Monad m => FreshT n m (Name n)
fresh
 = FreshT
 $ \ns ->
   case ns of
    NameState f s
     -> let (n,s') = f s
        in  return (NameState f s', n)

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

