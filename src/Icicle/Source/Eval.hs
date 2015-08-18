-- | Evaluation of Source programs.
--
--
-- Note: Numbers
-- ~~~~~~~~~~~~~
-- Perhaps surprisingly, the LitInt primitive does not necessarily return an Int.
-- Its type is "forall a. Num a => a", because it can be used as either an Int or a Double.
-- This means that when simply looking at a "LitInt n", it is not known whether
-- it should evaluate to "VInt n" or "VDouble n".
--
-- Operators like (+), (*) etc are polymorphic and can operate on Ints or Doubles.
-- However because of the above, we would have to also handle all combinations of Ints and Doubles.
--
-- As a workaround, just to simplify the evaluator, ALL numbers are treated as VDouble.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Eval (
    EvalError   (..)
  , evalQ
  , evalX
  ) where

import                  Icicle.Common.Base
import                  Icicle.Source.Query

import                  P
import                  Data.List (zip, nubBy, groupBy, reverse, take)
import qualified        Data.Map as Map

data EvalError a n
 = EvalErrorWindowWithNoDate a BaseValue
 | EvalErrorNoSuchVariable   a (Name n)
 | EvalErrorOpBadArgs        a Op  [BaseValue]
 | EvalErrorFunBadArgs       a Fun [BaseValue]

 | EvalErrorExpNeitherSort   a (Exp a n)

 | EvalErrorApplicationOfNonPrimitive a (Exp a n)
 deriving (Show, Eq, Ord)

type Record n
 = Map.Map (Name n) BaseValue


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
                 -> return $ VException ExceptFold1NoValue

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
    Lit (LitInt i)
     -> return (VDouble $ fromIntegral i)

    Lit (LitDouble i)
     -> return (VDouble i)

    Lit (LitString i)
     -> return (VString i)

    Fun f
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorFunBadArgs ann f args
            case f of
             Log
              | [VDouble i] <- args
              -> return $ VDouble $ log i
              | otherwise -> err
             Exp
              | [VDouble i] <- args
              -> return $ VDouble $ exp i
              | otherwise -> err
             -- Use Doubles as only number representation.
             -- See Note: Numbers
             ToDouble
              | [VDouble i] <- args
              -> return $ VDouble i
              | otherwise -> err
             ToInt
              | [VDouble i] <- args
              -> return $ VDouble $ fromIntegral (truncate i :: Int)
              | otherwise -> err

    Op o
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorOpBadArgs ann o args
            let isExcept v
                    | VException _ <- v
                    = True
                    | otherwise
                    = False
            case o of
             _
              -- Propagation of errors.
              | (xcept:_) <- filter isExcept args
              -> return xcept

             ArithDouble Div
              | [VDouble i, VDouble j] <- args
              -> return $ VDouble (i / j)
              | otherwise
              -> err

             ArithUnary Negate
              | [VDouble i] <- args
              -> return $ VDouble $ negate i
              | otherwise
              -> err

             ArithBinary Mul
              | [VDouble i, VDouble j] <- args
              -> return $ VDouble (i * j)
              | otherwise
              -> err

             ArithBinary Add
              | [VDouble i, VDouble j] <- args
              -> return $ VDouble (i + j)
              | otherwise
              -> err


             ArithBinary Sub
              | [VDouble i, VDouble j] <- args
              -> return $ VDouble (i - j)
              | otherwise
              -> err

             ArithBinary Pow
              | [VDouble i, VDouble j] <- args
              -> return $ VDouble (i ** j)
              | otherwise
              -> err


             Relation Lt
              | [i, j] <- args
              -> return $ VBool $ i < j
              | otherwise
              -> err

             Relation Le
              | [i, j] <- args
              -> return $ VBool $ i <= j
              | otherwise
              -> err

             Relation Gt
              | [i, j] <- args
              -> return $ VBool $ i > j
              | otherwise
              -> err

             Relation Ge
              | [i, j] <- args
              -> return $ VBool $ i >= j
              | otherwise
              -> err

             Relation Eq
              | [i, j] <- args
              -> return $ VBool $ i == j
              | otherwise
              -> err

             Relation Ne
              | [i, j] <- args
              -> return $ VBool $ i /= j
              | otherwise
              -> err

             TupleComma
              | [a, b] <- args
              -> return $ VPair a b
              | otherwise
              -> err


