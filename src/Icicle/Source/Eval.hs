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
 | EvalErrorOpBadArgs        Op  [BaseValue]
 | EvalErrorAggBadArgs       Agg [Exp n]

 | EvalErrorExpNeitherSort
 { evalErrorExp          :: Exp n }

 | EvalErrorApplicationOfNonPrimitive
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

                Let n x
                 -> let str = mapM (\v -> Map.insert n <$> evalX x [] v <*> return v) vs
                        agg = Map.insert n <$> evalX x vs env <*> return env
                    in  case (str, agg) of
                         (Right vs', Right env')
                          ->    evalQ q' vs' env'
                         (Right vs', _)
                          ->    evalQ q' vs' env
                         (_, Right env')
                          ->    evalQ q' vs  env'
                         (Left _, Left _)
                          -> Left $ EvalErrorExpNeitherSort x


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

    Nested q
     -> evalQ q vs env

    App{}
     | Just (p, xs) <- takePrimApps x
     -> evalP p xs vs env
     | otherwise
     -> Left $ EvalErrorApplicationOfNonPrimitive x

    Prim p
     -> evalP p [] vs env


evalP   :: Ord n
        => Prim
        -> [Exp n]
        -> [Record n]
        -> Record n
        -> Either (EvalError n) BaseValue
evalP p xs vs env
 = case p of
    Agg ag
     -> evalA ag xs vs env

    Lit (LitInt i)
     -> return (VInt i)

    Op o
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorOpBadArgs o args
            case o of
             Div
              | [VInt i, VInt j] <- args
              -> return $ VInt (i `div` j)
              | otherwise
              -> err

             Mul
              | [VInt i, VInt j] <- args
              -> return $ VInt (i * j)
              | otherwise
              -> err

             Add
              | [VInt i, VInt j] <- args
              -> return $ VInt (i + j)
              | otherwise
              -> err

             Sub
              | [VInt i, VInt j] <- args
              -> return $ VInt (i - j)
              | otherwise
              -> err

             Negate
              | [VInt i] <- args
              -> return $ VInt $ negate i
              | otherwise
              -> err




evalA   :: Ord n
        => Agg
        -> [Exp n]
        -> [Record n]
        -> Record n
        -> Either (EvalError n) BaseValue
evalA ag xs vs _env
 = case ag of
    Count
     | [] <- xs
     -> return $ VInt $ length vs
     | otherwise
     -> err

    Newest
     | [x] <- xs
     , Just v <- foldl (\_ v -> Just v) Nothing vs
     -> VSome <$> evalX x [] v
     | [_] <- xs
     -> return $ VNone
     | otherwise
     -> err

    Oldest
     | [x] <- xs
     , (v:_) <- vs
     -> VSome <$> evalX x [] v
     | [_] <- xs
     -> return $ VNone
     | otherwise
     -> err

 where
  err = Left $ EvalErrorAggBadArgs ag xs

