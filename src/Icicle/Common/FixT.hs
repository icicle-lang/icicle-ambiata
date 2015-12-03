-- | Monad transformer for evaluating to a fixpoint?
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.FixT (
      FixT   (..)
    , Progress (..)
    , fixpoint
    , once
    , fixOfMaybe
    , progress
    ) where

import              P

import              Control.Monad.Trans.Class

newtype FixT m a
 = FixT
 { runFixT :: m (a, Progress) }

data Progress
 = RunAgain
 | NoProgress
 deriving (Eq, Ord, Show)

fixpoint :: Monad m => (a -> FixT m a) -> a -> m a
fixpoint f a
 = do (a',prog) <- runFixT $ f a
      case prog of
       RunAgain   -> fixpoint f a'
       NoProgress -> return a'

once :: Monad m => FixT m a -> m a
once f
 = do (r,_) <- runFixT f
      return r

fixOfMaybe :: Monad m => (a -> m (Maybe a)) -> a -> FixT m a
fixOfMaybe f a
 = do a' <- lift $ f a
      case a' of
       Just a''-> progress a''
       Nothing -> return a

eitherProgress :: Progress -> Progress -> Progress
eitherProgress RunAgain _ = RunAgain
eitherProgress _ RunAgain = RunAgain
eitherProgress _ _        = NoProgress

progress :: Monad m => a -> FixT m a
progress a
 = FixT $ return (a, RunAgain)

instance Monad m => Monad (FixT m) where
 (>>=) p q
  = FixT
  $ do  (res,prog)   <- runFixT p
        (res',prog') <- runFixT $ q res
        return (res', eitherProgress prog prog')

 return a = FixT $ return (a, NoProgress)

instance Monad m => Functor (FixT m) where
 fmap f p
  = p >>= (return . f)

instance Monad m => Applicative (FixT m) where
 pure = return
 (<*>) f x
  = do f' <- f
       x' <- x
       return $ f' x'
 

instance MonadTrans FixT where
 lift m
  = FixT
  $ do  v <- m
        return (v, NoProgress)


