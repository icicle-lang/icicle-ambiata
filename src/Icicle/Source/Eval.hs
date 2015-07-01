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

data EvalError a n
 = EvalErrorWindowWithNoDate a BaseValue
 | EvalErrorNoSuchVariable   a n
 | EvalErrorOpBadArgs        a Op  [BaseValue]
 | EvalErrorAggBadArgs       a Agg [Exp a n]

 | EvalErrorExpNeitherSort   a (Exp a n)

 | EvalErrorApplicationOfNonPrimitive a (Exp a n)
 deriving (Show, Eq, Ord)

type Record n
 = Map.Map n BaseValue


evalQ   :: Ord n
        => Query a n
        -> [Record n]
        -> Record n
        -> Either (EvalError a n) BaseValue
evalQ q vs env
 = case contexts q of
    []
     -> evalX (final q) vs env

    (c:cs)
     -> let q' = q { contexts = cs }
        in  case c of
                Windowed _ _ _
                 -> let vs' = filter window vs
                        window _ = True -- TODO
                    in  evalQ q' vs' env

                Latest _ i
                 -> let vs' = reverse $ take i $ reverse vs
                    in  case evalQ q' vs' env of
                         Left _
                          -> VArray <$> mapM (evalQ q' []) vs'
                         Right v
                          -> return v

                GroupBy _ g
                 -> do  gs <- mapM (evalX g []) vs

                        let vgs  = gs `zip` vs
                        let vgs' = groupBy ((==) `on` fst) vgs
                        let vvs' = fmap (fmap snd) vgs'

                        VArray <$> mapM (\vs' -> evalQ q' vs' env) vvs'

                Distinct _ g
                 -> do  gs <- mapM (evalX g []) vs

                        let vgs  = gs `zip` vs
                        let vgs' = nubBy ((==) `on` fst) vgs
                        let vs'  = fmap snd vgs'

                        evalQ q' vs' env

                Filter _ p
                 -> do  let isTrue (VBool True) = True
                            isTrue _            = False

                        vs' <- filterM (\v -> isTrue <$> evalX p [] v) vs
                        evalQ q' vs' env

                LetFold _ f
                 | FoldTypeFoldl1 <- foldType f
                 , (z:vs') <- vs
                 -> do  z' <- evalX (foldInit f) [] z
                        let ins = Map.insert (foldBind f)
                        v' <- foldM (\a v -> evalX (foldWork f) [] (ins a v)) z' vs'

                        evalQ q' vs (ins v' env)

                 | FoldTypeFoldl  <- foldType f
                 -> do  z' <- evalX (foldInit f) [] env
                        let ins = Map.insert (foldBind f)
                        v' <- foldM (\a v -> evalX (foldWork f) [] (ins a v)) z' vs

                        evalQ q' vs (ins v' env)

                 | otherwise
                 -> return VNone

                Let a n x
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
                          -> Left $ EvalErrorExpNeitherSort a x


evalX   :: Ord n
        => Exp a n
        -> [Record n]
        -> Record n
        -> Either (EvalError a n) BaseValue
evalX x vs env
 = case x of
    Var a n
     | Just v <- Map.lookup n env
     -> return v
     | otherwise
     -> Left $ EvalErrorNoSuchVariable a n

    Nested _ q
     -> evalQ q vs env

    App a _ _
     | Just (p, a', xs) <- takePrimApps x
     -> evalP a' p xs vs env
     | otherwise
     -> Left $ EvalErrorApplicationOfNonPrimitive a x

    Prim a p
     -> evalP a p [] vs env


evalP   :: Ord n
        => a
        -> Prim
        -> [Exp a n]
        -> [Record n]
        -> Record n
        -> Either (EvalError a n) BaseValue
evalP ann p xs vs env
 = case p of
    Agg ag
     -> evalA ann ag xs vs env

    Lit (LitInt i)
     -> return (VInt i)

    Op o
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorOpBadArgs ann o args
            case o of
             _
              | any (==VNone) args
              -> return $ VNone
             Div
              | [_,      VInt 0] <- args
              -> return   VNone
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

             Lt
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i < j
              | otherwise
              -> err

             Le
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i <= j
              | otherwise
              -> err

             Gt
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i > j
              | otherwise
              -> err

             Ge
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i >= j
              | otherwise
              -> err

             Eq
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i == j
              | otherwise
              -> err

             Ne
              | [VInt i, VInt j] <- args
              -> return $ VBool $ i /= j
              | otherwise
              -> err




evalA   :: Ord n
        => a
        -> Agg
        -> [Exp a n]
        -> [Record n]
        -> Record n
        -> Either (EvalError a n) BaseValue
evalA ann ag xs vs _env
 = case ag of
    Count
     | [] <- xs
     -> return $ VInt $ length vs
     | otherwise
     -> err

    SumA
     | [x] <- xs
     -> do  v <- foldM (\a v -> do v' <- evalX x [] v
                                   case v' of
                                    VInt i
                                     | Just a' <- a
                                     -> return $ Just $ a' + i
                                     | Nothing <- a
                                     -> return Nothing
                                    VNone
                                     -> return Nothing
                                    _      -> err) Nothing vs
            return $ case v of
             Nothing -> VNone
             Just i  -> VInt i

     | otherwise
     -> err

    -- TODO: what happens if the "newest" is possibly VNone?
    Newest
     | [x] <- xs
     , Just v <- foldl (\_ v -> Just v) Nothing vs
     -> evalX x [] v
     | [_] <- xs
     -> return $ VNone
     | otherwise
     -> err

    Oldest
     | [x] <- xs
     , (v:_) <- vs
     -> evalX x [] v
     | [_] <- xs
     -> return $ VNone
     | otherwise
     -> err

 where
  err = Left $ EvalErrorAggBadArgs ann ag xs

