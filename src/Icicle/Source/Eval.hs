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
import qualified        Icicle.Data.Time                as DT

import                  P
import                  Data.List (zip, nubBy, groupBy, take)
import qualified        Data.Map as Map

data EvalError a n
 = EvalErrorNoSuchVariable   a (Name n)
 | EvalErrorPrimBadArgs      a Prim [BaseValue]

 | EvalErrorExpNeitherSort   a (Exp a n)

 | EvalErrorApplicationOfNonPrimitive a (Exp a n)

 | EvalErrorCaseExpressionNoMatch a (Exp a n) BaseValue
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

                GroupFold _ _ _ g
                 -> do  gs <- mapM (evalX g []) vs

                        let vgs  = gs `zip` vs
                        let vgs' = fmap snd vgs

                        evalQ q' vgs' env

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
                 -> return $ VError ExceptFold1NoValue

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

    Case _ scrut pats
     -> do scrut' <- evalX scrut vs env
           goPats scrut' pats

 where
  goPats v []
   = Left
   $ EvalErrorCaseExpressionNoMatch (annotOfExp x) x v
  goPats v ((p,xx):rest)
   | Just subst <- substOfPattern p v
   = evalX xx vs (Map.union subst env)
   | otherwise
   = goPats v rest


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

    Lit (LitTime i)
     -> return (VTime i)

    PrimCon con
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorPrimBadArgs ann p args
            case con of
             ConNone
              -> return VNone
             ConSome
              | [va] <- args
              -> return $ VSome va
              | otherwise
              -> err
             ConTuple
              | [va,vb] <- args
              -> return $ VPair va vb
              | otherwise
              -> err
             ConTrue
              -> return $ VBool True
             ConFalse
              -> return $ VBool False
             ConLeft
              | [va] <- args
              -> return $ VLeft va
              | otherwise
              -> err
             ConRight
              | [va] <- args
              -> return $ VRight va
              | otherwise
              -> err
             ConError ex
              -> return $ VError ex

    Fun f
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorPrimBadArgs ann p args
            case f of
             Log
              | [VDouble i] <- args
              -> return $ VDouble $ log i
              | otherwise -> err
             Exp
              | [VDouble i] <- args
              -> return $ VDouble $ exp i
              | otherwise -> err
             Sqrt
              | [VDouble i] <- args
              -> return $ VDouble $ sqrt i
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
             DaysBetween
              | [VTime i, VTime j] <- args
              -> return $ VDouble $ fromIntegral $ DT.daysDifference i j
              | otherwise -> err
             DaysEpoch
              | [VTime i] <- args
              -> return $ VDouble $ fromIntegral $ DT.daysOfTime i
              | otherwise -> err
             Seq
              | [VError e,_] <- args
              -> return $ VError e
              | [_,i] <- args
              -> return i
              | otherwise -> err
    Op o
     -> do  args <- mapM (\x' -> evalX x' vs env) xs
            let err = Left $ EvalErrorPrimBadArgs ann p args
            let isExcept v
                    | VError _ <- v
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

             LogicalUnary Not
              | [VBool i] <- args
              -> return $ VBool $ not i
              | otherwise
              -> err

             LogicalBinary And
              | [VBool i, VBool j] <- args
              -> return $ VBool $ i && j
              | otherwise
              -> err

             LogicalBinary Or
              | [VBool i, VBool j] <- args
              -> return $ VBool $ i || j
              | otherwise
              -> err

             TimeBinary DaysBefore
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusDays j $ truncate i
              | otherwise
              -> err

             TimeBinary DaysAfter
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusDays j $ negate $ truncate i
              | otherwise
              -> err

             TimeBinary WeeksBefore
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusDays j (7 * truncate i)
              | otherwise
              -> err

             TimeBinary WeeksAfter
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusDays j $ negate (7 * truncate i)
              | otherwise
              -> err

             TimeBinary MonthsBefore
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusMonths j $ truncate i
              | otherwise
              -> err

             TimeBinary MonthsAfter
              | [VDouble i, VTime j] <- args
              -> return $ VTime $ DT.minusMonths j $ negate $ truncate i
              | otherwise
              -> err

             TupleComma
              | [a, b] <- args
              -> return $ VPair a b
              | otherwise
              -> err


