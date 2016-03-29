-- | Check whether a query can be implemented as a "resumable computation"
-- with a finite lookbehind/history.
--
-- This ends up being quite a simple check (simpler than I expected):
-- we descend into the query, looking through function definitions,
-- and stop when we find a window, a latest, or a fold.
--
-- If we find a fold that is not surrounded by a window or latest,
-- it applies to all inputs, so has (or could have) infinite lookbehind.
--
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Resumable (
    checkResumableQ
  , checkResumableX
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Query


import                  P


import                  Data.Hashable (Hashable)
import qualified        Data.Map as Map



type Result a n = Either (CheckError a n) ()

checkResumableQ
        :: (Hashable n, Eq n)
        => CheckEnv a n
        -> Query    a n
        -> Result   a n
checkResumableQ ctx (Query [] x)
 = checkResumableX ctx x

checkResumableQ ctx q@(Query (c:cs) xfinal)
 = case c of
    -- Windows and latests are ok!
    Windowed{}
     -> return ()
    Latest{}
     -> return ()
    -- We needn't check the key: it is an Element
    -- (This will change if/when we implement Scans)
    -- (It wouldn't hurt if we *did* check the expression)
    GroupBy{}
     -> go
    -- As above
    Distinct{}
     -> go

    -- For group-fold, the expression is the actual group:
    -- this expression needs to be checked.
    -- The remainder of the query doesn't need to be checked though,
    -- because it is operating over the group map.
    GroupFold _ _ _ x
     -> goX x

    -- Filter expressions, like groups, must be Element
    Filter{}
     -> go

    -- Lets can be anything, so must be checked
    Let _ _ x
     -> goX x >> go

    -- A let here is an error!
    LetFold a _
     -> errorSuggestions
         (ErrorResumableFoldNotAllowedHere a q)
         [ Suggest "For very large data, we cannot perform folds over all the data"
         , Suggest "For this reason, we require all folds to be in a windowed or latest " ]

 where
  q' = Query cs xfinal
  go = checkResumableQ ctx q'
  goX = checkResumableX ctx


checkResumableX
        :: (Hashable n, Eq n)
        => CheckEnv a n
        -> Exp      a n
        -> Result   a n
checkResumableX ctx x
 = case x of
    Var _ n
     | Just fun <- Map.lookup n $ checkBodies ctx
     -> checkResumableQ ctx $ body fun
     | otherwise
     -> return ()
    Nested _ q
     -> checkResumableQ ctx q
    App _ p q
     -> checkResumableX ctx p >> checkResumableX ctx q
    Prim{}
     -> return ()
    Case _ s ps
     -> checkResumableX ctx s >> mapM_ (checkResumableX ctx . snd) ps

