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
import                  Data.Hashable (Hashable)
import qualified        Data.Map as Map



type Result a n = Either (CheckError a n) ()

invariantQ
        :: (Hashable n, Eq n)
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
     -> errBanGroup "Consider moving the window to the start of the query"
    Latest{}
     | allowLatest inv
     -- XXX: ban latest inside latests.
     -- This is because latest needs access to the current fact identifier, but this isn't captured in the latest buffer.
     -- This is a bug in the code generator and should be fixed.
     -> goBanAll
     | otherwise
     -> errBanLatest
    GroupBy _ x
     | allowWindowsOrGroups inv
     -> goX x >> goBanGroup
     | otherwise
     -> errBanGroup "Nested groups are not supported"
    Distinct _ x
     | allowWindowsOrGroups inv
     -> goX x >> goBanGroup
     | otherwise
     -> errBanGroup "Nested Distinct are not supported"

    GroupFold _ _ _ x
     | allowWindowsOrGroups inv
     -> goX x >> goBanAll
     | otherwise
     -> errBanGroup "Nested group folds are not supported"

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

  goBanAll
     = invariantQ
        (ctx{ checkInvariants = inv{ allowLatest = False, allowWindowsOrGroups = False }})
        q'

  goBanGroup
     = invariantQ
        (ctx{ checkInvariants = inv{ allowWindowsOrGroups = False }})
        q'

  errBanLatest
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Latest is not allowed inside group-fold or another latest."
      , Suggest "If you are using latest inside a latest, you should be able to rewrite your query to use a single latest."
      , Suggest "Note that 'newest' is implemented using latest, but if you have `latest 5 ~> newest value` you might be able to just use `newest value`."]

  errBanGroup sug
   = errorSuggestions
      (ErrorContextNotAllowedHere (annotOfContext c) c)
      [ Suggest "Groups, distincts, latests and windows cannot be nested"
      , Suggest sug]


invariantX
        :: (Hashable n, Eq n)
        => CheckEnv a n
        -> Exp      a n
        -> Result   a n
invariantX ctx x
 = case x of
    Var a n
     -> goFun a n []
    Nested _ q
     -> invariantQ ctx q
    App{}
     | (f,xs) <- takeApps x
     , Var a n <- f
     -> goFun a n xs
    App _ p q
     -> invariantX ctx p >> invariantX ctx q
    Prim{}
     -> return ()
    Case _ s ps
     -> invariantX ctx s >> mapM_ (invariantX ctx . snd) ps

 where
  goFun a n args
   | Just fun <- Map.lookup n $ checkBodies ctx
   = let ctx' = foldl bindArg ctx (arguments fun `zip` args)
     in  errorInFunctionEither a n
       $ invariantQ ctx' $ body fun
   | otherwise
   = mapM_ (invariantX ctx) args

  bindArg ctx' ((_,n),def)
   = ctx'
   { checkBodies = Map.insert n (Function [] (Query [] def))
                 $ checkBodies ctx'
   }

