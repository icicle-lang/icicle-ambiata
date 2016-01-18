{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Program.Subst (
      unsafeSubstStreams
    , unsafeSubstStreams'
    , unsafeSubstSnds
    , unsafeSubstExp
    , unsafeSubstExp'
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp hiding (Exp)
import Icicle.Core.Exp
import Icicle.Core.Stream.Stream

import              P hiding (error)
import              Prelude (error)


unsafeSubstStreams
    :: Ord n
    => [(Name n, Name n)]
    -> [Stream a n]
    -> [Stream a n]
unsafeSubstStreams sub ts
 = foldl (flip $ uncurry unsafeSubstStreams') ts sub

-- | Rename all uses of stream to another name
unsafeSubstStreams'
    :: Ord n
    => Name n
    -> Name n
    -> [Stream a n]
    -> [Stream a n]
unsafeSubstStreams' from to ss
 = fmap subStream ss

 where
  subStream s
   = case s of
      SFold n vt z k
       -> SFold n vt (subX z) (subX k)
      SFilter x subs
       -> SFilter (subX x) (fmap subStream subs)

  subX = unsafeSubstExp' from to


unsafeSubstSnds
    :: Ord n
    => [(Name n, Name n)]
    -> [(b, Exp a n)]
    -> [(b, Exp a n)]
unsafeSubstSnds sub
 = fmap (onSnd $ unsafeSubstExp sub)
 where
  onSnd f (a,b) = (a, f b)

unsafeSubstExp
    :: Ord n
    => [(Name n, Name n)]
    -> Exp a n
    -> Exp a n
unsafeSubstExp sub x
 = foldl (flip $ uncurry unsafeSubstExp') x sub


-- | Unsafe substitution that assumes the name "to" is never shadowed.
-- This should be the case for any well formed Core program.
unsafeSubstExp' :: Ord n => Name n -> Name n -> Exp a n -> Exp a n
unsafeSubstExp' from to x
 = let ann     = annotOfExp x
   in  case substMaybe from (XVar ann to) x of
        Just x' -> x'
        Nothing -> error "IMPOSSIBLE: unsafeSubstExp name being shadowed, cannot perform substitution"


