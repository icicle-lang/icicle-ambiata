-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten (
    flatten
  , FlattenError(..)
  ) where

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat

import qualified    Icicle.Core.Exp.Prim           as Core
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              Icicle.Internal.Pretty

import              P
import              Control.Monad.Trans.Class

import qualified    Data.Map                       as Map
import              Data.List (reverse)


data FlattenError n
 = FlattenErrorApplicationNonPrimitive (Exp n Core.Prim)
 | FlattenErrorBareLambda (Exp n Core.Prim)
 | FlattenErrorPrimBadArgs Core.Prim [Exp n Core.Prim]
 deriving (Eq, Ord, Show)

type FlatM n
 = FreshT n (Either (FlattenError n)) (Statement n Flat.Prim)


-- | Flatten the primitives in a statement.
-- This just calls @flatX@ for every expression, wrapping the statement.
flatten :: (Ord n, Pretty n)
        => Statement n Core.Prim
        -> FlatM n
flatten s
 = case s of
    If x ts es
     -> flatX x
     $ \x'
     -> If x' <$> flatten ts <*> flatten es

    Let n x ss
     -> flatX x
     $ \x'
     -> Let n x' <$> flatten ss

    ForeachInts n from to ss
     -> flatX from
     $ \from'
     -> flatX to
     $ \to'
     -> ForeachInts n from' to' <$> flatten ss

    ForeachFacts n vt lo ss
     -> ForeachFacts n vt lo <$> flatten ss

    Block ss
     -> Block <$> mapM flatten ss

    InitAccumulator acc ss
     -> flatX (accInit acc)
     $ \x'
     -> InitAccumulator (acc { accInit = x' }) <$> flatten ss

    Read n m ss
     -> Read n m <$> flatten ss

    Write n x
     -> flatX x
     $ \x'
     -> return $ Write n x'

    Push n x
     -> flatX x
     $ \x'
     -> return $ Push n x'

    Return x
     -> flatX x
     $ \x'
     -> return $ Return x'

    KeepFactInHistory
     -> return $ KeepFactInHistory



-- | Flatten an expression, wrapping the statement with any lets or loops or other bindings
-- The statement function takes the new expression.
flatX   :: (Ord n, Pretty n)
        => Exp n Core.Prim
        -> (Exp n Flat.Prim -> FlatM n)
        -> FlatM n

flatX xx stm
 = convX
 where
  -- Do a bit of simplification.
  -- Betas must be converted to lets even if they are not simple values:
  -- Unapplied lambdas aren't really able to be converted, since
  -- we're lifting some expressions to statements, and there is no statement-level
  -- lambda, only expression-level lambda.
  x' = beta isSimpleValue
     $ betaToLets xx

  -- Convert the simplified expression.
  convX
   = case x' of
      -- If it doesn't do anything interesting, we can just call the statement
      -- with the original expression
      XVar n
       -> stm $ XVar n
      XValue vt bv
       -> stm $ XValue vt bv

      XApp{}
       -- Primitive applications are where it gets interesting.
       -- See below
       | Just (p,xs) <- takePrimApps x'
       -> flatPrim p xs

       -- What is the function of this application?
       --
       -- It's not a primitive.
       -- It's not a lambda, since we did betaToLets above.
       -- It's not a value, since that wouldn't typecheck
       -- It's not a let-bound variable, since lets can't bind funs & so wouldn't typecheck.
       --
       -- Therefore, this should not happen for a valid program.
       | otherwise
       -> lift $ Left $ FlattenErrorApplicationNonPrimitive x'

      XPrim p
       -> flatPrim p []

      -- Unapplied lambda: this should not happen for a well-typed program
      XLam{}
       -> lift $ Left $ FlattenErrorBareLambda x'


      -- Convert expression lets into statement lets
      XLet n p q
       -> flatX p
       $ \p'
       -> Let n p' <$> flatX q stm


  -- Handle primitive applications.
  -- PrimFolds get turned into loops and whatnot
  flatPrim p xs
   = case p of
      -- Arithmetic and simple stuff are easy, just move it over
      Core.PrimMinimal pm
       -> primApps (Flat.PrimMinimal pm) xs []

      -- Handle folds below
      Core.PrimFold pf ta
       -> flatFold pf ta xs

      -- Map: insert value into map, or if key already exists,
      -- apply update function to existing value
      Core.PrimMap (Core.PrimMapInsertOrUpdate tk tv)
       | [upd, ins, key, map]   <- xs
       -> flatX key
       $ \key'
       -> flatX map
       $ \map'
       -> let fpLookup    = XPrim (Flat.PrimProject (Flat.PrimProjectMapLookup tk tv))
              fpIsSome    = XPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome tv))
              fpOptionGet = XPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet tv))
              fpUpdate    = XPrim (Flat.PrimUpdate (Flat.PrimUpdateMapPut tk tv))

              update val
                     =  slet    (fpOptionGet `XApp` val)                $ \val'
                     -> flatX   (upd `XApp` val')                       $ \upd'
                     -> slet    (makeApps fpUpdate [map', key', upd'])  $ \map''
                     -> stm map''

              insert
                     =  flatX   ins                                     $ \ins'
                     -> slet    (makeApps fpUpdate [map', key', ins'])  $ \map''
                     -> stm map''

         in slet (makeApps fpLookup [map', key'])                       $ \val
         ->  If (fpIsSome `XApp` val)
                <$> update val
                <*> insert

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tk tv tv')
       | [upd, map]   <- xs
       -> flatX map
       $ \map'
       -> do    accN <- fresh
                let fpMapLen   = XPrim (Flat.PrimProject $ Flat.PrimProjectMapLength tk tv)
                let fpMapIx    = XPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeMapIndex   tk tv)
                let fpUpdate   = XPrim (Flat.PrimUpdate  $ Flat.PrimUpdateMapPut     tk tv')

                stm' <- stm (XVar accN)

                loop <- forI (fpMapLen `XApp` map')                 $ \iter
                     -> fmap    (Read accN accN)                    $
                        slet    (fpMapIx `makeApps` [map', iter])   $ \elm
                     -> slet    (proj False tk tv elm)              $ \efst
                     -> slet    (proj True  tk tv elm)              $ \esnd
                     -> flatX   (upd `XApp` esnd)                   $ \esnd'
                     -> slet    (fpUpdate `makeApps` [XVar accN, efst, esnd']) $ \map''
                     -> return  (Write accN map'')


                let mapT = MapT tk tv'
                return $ InitAccumulator
                            (Accumulator accN Mutable mapT $ XValue mapT $ VMap Map.empty)
                            (loop <> Read accN accN stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map over array: create new empty array, for each element, etc
      Core.PrimArray (Core.PrimArrayMap ta tb)
       | [upd, arr]   <- xs
       -> flatX arr
       $ \arr'
       -> do    accN <- fresh
                let fpArrLen   = XPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength ta)
                let fpArrIx    = XPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   ta)
                let fpArrNew   = XPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayCreate  tb)
                let fpUpdate   = XPrim (Flat.PrimUpdate  $ Flat.PrimUpdateArrayPut     tb)

                stm' <- stm (XVar accN)

                loop <- forI (fpArrLen `XApp` arr')                 $ \iter
                     -> fmap    (Read accN accN)                    $
                        slet    (fpArrIx `makeApps` [arr', iter])   $ \elm
                     -> flatX   (upd `XApp` elm)                    $ \elm'
                     -> slet    (fpUpdate `makeApps` [XVar accN, iter, elm']) $ \arr''
                     -> return  (Write accN arr'')


                let arrT = ArrayT tb
                return $ InitAccumulator
                            (Accumulator accN Mutable arrT (fpArrNew `XApp` (fpArrLen `XApp` arr')))
                            (loop <> Read accN accN stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


  -- Convert arguments to a simple primitive.
  -- conv is what we've already converted
  primApps p [] conv
   = stm
   $ makeApps (XPrim p)
   $ reverse conv

  primApps p (a:as) conv
   = flatX a
   $ \a'
   -> primApps p as (a' : conv)

  -- Create a let binding with a fresh name
  slet x ss
   = do n  <- fresh
        Let n x <$> ss (XVar n)

  -- For loop with fresh name for iterator
  forI to ss
   = do n  <- fresh
        ForeachInts n (XValue IntT (VInt 0)) to <$> ss (XVar n)

  -- Handle primitive folds
  --
  -- Bool is just an if
  flatFold Core.PrimFoldBool _ [then_, else_, pred]
   -- XXX: we are using "stm" twice here,
   -- so duplicating branches.
   -- I don't think this is a biggie
   -- (yet)
   = flatX pred
   $ \pred'
   -> If pred'
        <$> flatX then_ stm
        <*> flatX else_ stm

  -- Array fold becomes a loop
  flatFold (Core.PrimFoldArray telem) tacc [k, z, arr]
   = do accN <- fresh
        stm' <- stm (XVar accN)

        let fpArrayLen = XPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength telem)
        let fpArrayIx  = XPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   telem)

        -- Loop body updates accumulator with k function
        loop <-  flatX arr                                  $ \arr'
             ->  forI   (fpArrayLen `XApp` arr')            $ \iter
             ->  fmap   (Read accN accN)                    $
                 slet   (fpArrayIx `makeApps` [arr', iter]) $ \elm
             ->  flatX  (makeApps k [XVar accN, elm])       $ \x
             ->  return (Write accN x)

        -- Initialise accumulator with value z, execute loop, read from accumulator
        flatX z $ \z' ->
            return (InitAccumulator (Accumulator accN Mutable tacc z')
                   (loop <> Read accN accN stm'))


  -- Fold over map. Very similar to above
  flatFold (Core.PrimFoldMap tk tv) tacc [k, z, arr]
   = do accN <- fresh
        stm' <- stm (XVar accN)

        let fpMapLen   = XPrim (Flat.PrimProject $ Flat.PrimProjectMapLength tk tv)
        let fpMapIx    = XPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeMapIndex   tk tv)

        -- Loop is the same as for array, except we're grabbing the keys and values
        loop <- flatX arr                                   $ \arr'
             -> forI    (fpMapLen `XApp` arr')              $ \iter
             -> fmap    (Read accN accN)                    $
                slet    (fpMapIx `makeApps` [arr', iter])   $ \elm
             -> slet    (proj False tk tv elm)              $ \efst
             -> slet    (proj True  tk tv elm)              $ \esnd
             -> flatX   (makeApps k [XVar accN, efst, esnd])$ \x
             -> return  (Write accN x)

        flatX z $ \z' ->
            return (InitAccumulator (Accumulator accN Mutable tacc z')
                   (loop <> Read accN accN stm'))


  -- Fold over an option is just "maybe" combinator.
  flatFold (Core.PrimFoldOption ta) _ [xsome, xnone, opt]
   = let fpIsSome    = XPrim (Flat.PrimProject  (Flat.PrimProjectOptionIsSome ta))
         fpOptionGet = XPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeOptionGet     ta))
     in  flatX opt
      $ \opt'
      -- If we have a value
      -> If (fpIsSome `XApp` opt')
         -- Rip the value out and apply it
         <$> slet (fpOptionGet `XApp` opt')
             (\val -> flatX (xsome `XApp` val) stm)

         -- There's no value so return the none branch
         <*> flatX xnone stm

  -- None of the above cases apply, so must be bad arguments
  flatFold pf rt xs
   = lift $ Left $ FlattenErrorPrimBadArgs (Core.PrimFold pf rt) xs

  -- Create a fst# or snd#
  proj t ta tb e
   = let pm | not t
            = Min.PrimPairFst ta tb
            | otherwise
            = Min.PrimPairSnd ta tb
     in (XPrim $ Flat.PrimMinimal $ Min.PrimPair $ pm) `XApp` e

