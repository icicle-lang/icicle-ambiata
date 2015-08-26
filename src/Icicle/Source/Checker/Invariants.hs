-- | Check invariants of a query
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Invariants (
    invariantQ
  , invariantX
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Query


import                  P


import                  Data.List (zip)
import qualified        Data.Map as Map



type Result a n = Either (CheckError a n) ()

invariantQ
        :: Ord        n
        => CheckEnv a n
        -> Query    a n
        -> Result   a n
invariantQ ctx (Query [] x)
 = invariantX ctx x

invariantQ ctx (Query (c:cs) xfinal)
 = case c of
    Windowed{}
     | allowWindowsOrGroups inv
     -> go
     | otherwise
     -> errNotAllowed "Consider moving the window to the start of the query"
    Latest{}
     | allowWindowsOrGroups inv
     -> goNotAllowed
     | otherwise
     -> errNotAllowed "Latest inside a group or distinct is not currently supported. Latest inside a latest is silly, and also not supported"
    GroupBy _ x
     | allowWindowsOrGroups inv
     -> goX x >> goNotAllowed
     | otherwise
     -> errNotAllowed "Nested groups are not supported"
    Distinct _ x
     | allowWindowsOrGroups inv
     -> goX x >> goNotAllowed
     | otherwise
     -> errNotAllowed "Nested Distinct are not supported"

    Filter _ x
     -> goX x >> go
    LetFold _ f
     -> goX (foldInit f) >> goX (foldWork f) >> go
    Let _ _ x
     -> goX x >> go

 where
  inv = checkInvariants ctx
  q' = Query cs xfinal
  go = invariantQ ctx q'
  goX = invariantX ctx

  goNotAllowed
     = invariantQ
        (ctx{ checkInvariants = inv{ allowWindowsOrGroups = False }})
        q'

  errNotAllowed sug
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Groups, distincts, latests and windows cannot be nested"
      , Suggest sug]


invariantX
        :: Ord        n
        => CheckEnv a n
        -> Exp      a n
        -> Result   a n
invariantX ctx x
 = case x of
    Var _ n
     -> goFun n []
    Nested _ q
     -> invariantQ ctx q
    App{}
     | (f,xs) <- takeApps x
     , Var _ n <- f
     -> goFun n xs
    App _ p q
     -> invariantX ctx p >> invariantX ctx q
    Prim{}
     -> return ()
    Case _ s ps
     -> invariantX ctx s >> mapM_ (invariantX ctx . snd) ps

 where
  goFun n args
   | Just fun <- Map.lookup n $ checkBodies ctx
   = let ctx' = foldl bindArg ctx (arguments fun `zip` args)
     -- TODO: wrap error in function information
     in  invariantQ ctx' $ body fun
   | otherwise
   = mapM_ (invariantX ctx) args

  bindArg ctx' ((_,n),def)
   = ctx'
   { checkBodies = Map.insert n (Function [] (Query [] def))
                 $ checkBodies ctx'
   }

