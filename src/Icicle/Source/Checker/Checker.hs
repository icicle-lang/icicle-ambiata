{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  , checkX
  ) where

import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Type as T

import                  P

import qualified        Data.Map as Map


type FeatureMap n = Map.Map n (Map.Map n BaseType)
type Env        n = Map.Map n UniverseType
type Result     n = Either (CheckError n) UniverseType


checkQT :: Ord n
        => FeatureMap n
        -> QueryTop   n
        -> Result     n
checkQT features qt
 = case Map.lookup (feature qt) features of
    Just f
     -> checkQ (Map.map (UniverseType Elem) f) (query qt)
    Nothing
     -> Left $ ErrorNoSuchFeature (feature qt)


checkQ  :: Ord      n
        => Env      n
        -> Query    n
        -> Result   n
checkQ ctx q
 = case contexts q of
    []
     -> checkX ctx (final q) >>= requireAggOrGroup
    (c:cs)
     -> let q' = q { contexts = cs }
            tq = checkQ ctx q'

        in  case c of
             Windowed _ _
              -- TODO: check that range is valid
              -> tq

             Latest _
              -> tq >>= wrapAsAgg

             GroupBy e
              -> do te <- checkX ctx e
                    -- Check that the thing we're grouping by is enum-ish
                    expIsEnum c te
                    -- And contains no aggregates
                    expIsElem c te

                    t' <- tq
                    -- The group contents must be an aggregate.
                    -- No nested groups
                    when (not $ isAgg $ universe t')
                     $ Left $ ErrorReturnNotAggregate q' t'

                    return (UniverseType (Group $ baseType te) (baseType t'))

             Distinct e
              -> do te <- checkX ctx e
                    expIsEnum c te
                    expIsElem c te
                    tq

             Filter   e
              -> do te <- checkX ctx e
                    expIsBool c te
                    expIsElem c te
                    tq

             LetFold f
              -> do ti <- checkX ctx  $ foldInit f
                    expIsElem c ti
                    let ctx' = Map.insert (foldBind f) ti ctx
                    tw <- checkX ctx' $ foldWork f

                    when (ti /= tw)
                      $ Left $ ErrorTypeMismatch ti tw

                    expIsElem c tw

                    let ctx'' = Map.insert (foldBind f) (UniverseType AggU $ baseType ti) ctx
                    t' <- checkQ ctx'' q'
                    return (t' { baseType = T.OptionT $ baseType t' })


             Let n e
              -> do te <- checkX ctx e
                    let ctx' = Map.insert n te ctx
                    checkQ ctx' q'

 where
  expIsBool c te
   | T.BoolT <- baseType te
   = return ()
   | otherwise
   = Left $ ErrorContextExpNotBool c te

  expIsEnum c te
   = when (not $ isEnum $ baseType te)
         $ Left $ ErrorContextExpNotEnum c te

  expIsElem c te
   = when (not $ isPureOrElem $ universe te)
         $ Left $ ErrorContextExpNotElem c te

  requireAggOrGroup t
   | isPureOrElem $ universe t
   = Left $ ErrorReturnNotAggregate q t
   | otherwise
   = return t

  wrapAsAgg t
   | isPureOrElem $ universe t
   = return (UniverseType AggU $ T.ArrayT $ baseType t)
   | otherwise
   = return t


checkX  :: Ord      n
        => Env      n
        -> Exp      n
        -> Result   n
checkX ctx x
 | Just (prim, args) <- takePrimApps x
 = do ts <- mapM (checkX ctx) args
      checkP x prim ts
 | otherwise
 = case x of
    Var n
     -> maybe (Left $ ErrorNoSuchVariable n) (return)
              (Map.lookup n ctx)
    Nested q
     -> checkQ ctx q

    Prim p
     -> checkP x p []

    App{}
     -> Left $ ErrorApplicationOfNonPrim x


checkP  :: Ord      n
        => Exp      n
        -> Prim
        -> [UniverseType]
        -> Result   n
checkP x p args
 = case p of
    Op o
     | Negate <- o
     -> unary
     | otherwise
     -> binary

    Agg a
     | Count <- a
     , [] <- args
     -> return $ UniverseType AggU T.IntT
     | Newest <- a
     , [t] <- args
     , canCast (universe t) Elem
     -> return $ UniverseType AggU (T.OptionT $ baseType t)
     | Oldest <- a
     , [t] <- args
     , canCast (universe t) Elem
     -> return $ UniverseType AggU (T.OptionT $ baseType t)

     | otherwise
     -> err

    Lit (LitInt _)
     | [] <- args
     -> return $ UniverseType Pure T.IntT
     | otherwise
     -> err
 where
  err = Left $ ErrorPrimBadArgs x args

  unary
   | [t] <- args
   , baseType t == T.IntT
   , notGroup $ universe t
   = return t
   | otherwise
   = err

  binary
   | [a, b] <- args
   , baseType a == T.IntT
   , baseType b == T.IntT
   , Just u <- maxOf (universe a) (universe b)
   , notGroup u
   = return $ UniverseType u T.IntT
   | otherwise
   = err

  maxOf a b
   | canCast a b
   = Just b
   | canCast b a
   = Just a
   | otherwise
   = Nothing

  canCast a b
   | a == Pure
   = True
   | a == b
   = True
   | otherwise
   = False

  notGroup a
   | Group _ <- a
   = False
   | otherwise
   = True

