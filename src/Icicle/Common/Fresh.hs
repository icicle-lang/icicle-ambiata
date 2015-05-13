-- | Generating Fresh names
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExistentialQuantification #-}
module Icicle.Common.Fresh (
      Fresh    (..)
    , NameState
    , mkNameState
    , counterNameState
    , counterPrefixNameState
    , fresh
    ) where

import              Icicle.Common.Base
import              P

import qualified    Data.Text as T

newtype Fresh n a
 = Fresh
 { runFresh :: NameState n -> (NameState n, a) }

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

fresh :: Fresh n (Name n)
fresh
 = Fresh
 $ \ns ->
   case ns of
    NameState f s
     -> let (n,s') = f s
        in  (NameState f s', n)

instance Monad (Fresh n) where
 (>>=) p q
  = Fresh
  $ \ns ->
     let (ns',a) = runFresh p ns
     in runFresh (q a) ns'

 return a = Fresh $ \ns -> (ns, a)

instance Functor (Fresh n) where
 fmap f p
  = p >>= (return . f)

