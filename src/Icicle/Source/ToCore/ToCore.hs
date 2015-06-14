{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.ToCore (
    convertQueryTop
  , convertQuery
  , convertArray
  , convertReduce
  , convertGroupBy
  , convertExp
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Type as T

import                  P

import                  Control.Monad.Trans.Class
import qualified        Data.Map as Map
import                  Data.List (drop)


convertQueryTop
        :: Ord n
        => QueryTop n
        -> ConvertM n (C.Program n)
convertQueryTop _qt
 = lift $ Left ConvertErrorTODO


convertQuery
        :: Ord n
        => n
        -> Query n
        -> ConvertM n (CoreBinds n, n)
convertQuery n q
 = case contexts q of
    []
     -> convertReduce n (final q)

    -- TODO: need to convert "filter blah ~> windowed blah" into "windowed blah ~> filter blah"
    (Windowed (Days days) Nothing : _)
     -> do  n'      <- fresh
            (bs, b) <- convertQuery n' q'
            let bs'  = stream n' (C.SourceWindowed days) <> bs
            return (bs', b)
    -- TODO: support other ranges
    (Windowed _ _ : _)
     -> lift $ Left ConvertErrorTODO

    (Latest i : _)
     -> do  n'      <- fresh
            (bs, b) <- convertArray n' q'
            let bs'  = reduce n' (C.RLatest todoType (CE.constI i) n) <> bs
            return (bs', b)

    (GroupBy e : _)
     -> do  n'      <- fresh
            (f,z)   <- convertGroupBy q'
            e'      <- convertExp e


        
 where
  q' = q { contexts = drop 1 $ contexts q }

  todoType = T.IntT
