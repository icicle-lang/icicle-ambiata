{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Eval (
    EvalError   (..)
  , evalQ
  , evalX
  , evalA
  ) where

import                  Icicle.Common.Base
import                  Icicle.Source.Query

import                  P
import                  Data.List (zip, nubBy, groupBy, reverse, take)
import qualified        Data.Map as Map

data EvalError n
 = EvalErrorWindowWithNoDate BaseValue
 | EvalErrorNoSuchVariable   n
 | EvalErrorOpBadArgs        Op [BaseValue]

 | EvalErrorExpWrongSort
 { evalErrorExp          :: Exp n
 , evalErrorExpectedSort :: Sort }

 | EvalErrorExpNeitherSort
 { evalErrorExp          :: Exp n }
 deriving (Show, Eq, Ord)

type Record n
 = Map.Map n BaseValue


evalQ   :: Ord n
        => Query n
        -> [Record n]
        -> Record n
        -> Either (EvalError n) BaseValue
evalQ q vs env
 = case contexts q of
    []
     -> evalX (final q) vs env

    (c:cs)
     -> let q' = q { contexts = cs }
        in  case c of
                Windowed _ _
                 -> let vs' = filter window vs
                        window _ = True -- TODO
                    in  evalQ q' vs' env

                Latest i
                 -> let vs' = reverse $ take i $ reverse vs
                    in  evalQ q' vs' env

                GroupBy g
                 -> do  gs <- mapM (evalX g []) vs

                        let vgs  = gs `zip` vs
                        let vgs' = groupBy ((==) `on` fst) vgs
                        let vvs' = fmap (fmap snd) vgs'

                        VArray <$> mapM (\vs' -> evalQ q' vs' env) vvs'

                Distinct g
                 -> do  gs <- mapM (evalX g []) vs

                        let vgs  = gs `zip` vs
                        let vgs' = nubBy ((==) `on` fst) vgs
                        let vs'  = fmap snd vgs'

                        evalQ q' vs' env

                Filter p
                 -> do  let isTrue (VBool True) = True
                            isTrue _            = False

                        vs' <- filterM (\v -> isTrue <$> evalX p [] v) vs
                        evalQ q' vs' env

                LetFold f
                 | (z:vs') <- vs
                 -> do  z' <- evalX (foldInit f) [] z
                        let ins = Map.insert (foldBind f)
                        v' <- foldM (\a v -> evalX (foldWork f) [] (ins a v)) z' vs'

                        VSome <$> evalQ q' vs (ins v' env)
                 | otherwise
                 -> return VNone

                Let s n x
                 -> let str = mapM (\v -> Map.insert n <$> evalX x [] v <*> return v) vs
                        agg = Map.insert n <$> evalX x vs env <*> return env
                    in  case s of
                         Nothing
                          | Right vs' <- str
                          , Right env' <- agg
                          ->    evalQ q' vs' env'
                          | Right vs' <- str
                          ->    evalQ q' vs' env
                          | Right env' <- agg
                          ->    evalQ q' vs  env'
                          | otherwise
                          -> Left $ EvalErrorExpNeitherSort x

                         Just Stream
                          -> do vs' <- str
                                evalQ q' vs' env
                         Just Aggregate
                          -> do env' <- agg
                                evalQ q' vs  env'


evalX   :: Ord n
        => Exp n
        -> [Record n]
        -> Record n
        -> Either (EvalError n) BaseValue
evalX x vs env
 = case x of
    Var n
     | Just v <- Map.lookup n env
     -> return v
     | otherwise
     -> Left $ EvalErrorNoSuchVariable n

    Agg ag
     -> evalA ag vs env

    Nested q
     -> evalQ q vs env

    Op o xs
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorOpBadArgs o args
            case o of
             Div
              | [VInt i, VInt j] <- args
              -> return $ VInt (i `div` j)
              | otherwise
              -> err
     



evalA   :: Ord n
        => Agg n
        -> [Record n]
        -> Record n
        -> Either (EvalError n) BaseValue
evalA ag vs _env
 = case ag of
    Count
     -> return $ VInt $ length vs

    Newest x
     | Just v <- foldl (\_ v -> Just v) Nothing vs
     -> VSome <$> evalX x [] v
     | otherwise
     -> return $ VNone
     
    Oldest x
     | (v:_) <- vs
     -> VSome <$> evalX x [] v
     | otherwise
     -> return $ VNone


