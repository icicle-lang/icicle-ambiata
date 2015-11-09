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

import qualified    Data.List                      as List
import qualified    Data.Map                       as Map


data FlattenError a n
 = FlattenErrorApplicationNonPrimitive (Exp a n Core.Prim)
 | FlattenErrorBareLambda (Exp a n Core.Prim)
 | FlattenErrorPrimBadArgs Core.Prim [Exp a n Core.Prim]
 deriving (Eq, Ord, Show)

type FlatM a n
 = FreshT n (Either (FlattenError a n)) (Statement a n Flat.Prim)


-- | Flatten the primitives in a statement.
-- This just calls @flatX@ for every expression, wrapping the statement.
flatten :: (Ord n, Pretty n)
        => a
        -> Statement a n Core.Prim
        -> FlatM a n
flatten a_fresh s
 = case s of
    If x ts es
     -> flatX a_fresh x
     $ \x'
     -> If x' <$> flatten a_fresh ts <*> flatten a_fresh es

    Let n x ss
     -> flatX a_fresh x
     $ \x'
     -> Let n x' <$> flatten a_fresh ss

    ForeachInts n from to ss
     -> flatX a_fresh from
     $ \from'
     -> flatX a_fresh to
     $ \to'
     -> ForeachInts n from' to' <$> flatten a_fresh ss

    ForeachFacts ns vt lo ss
     -> ForeachFacts ns vt lo <$> flatten a_fresh ss

    Block ss
     -> Block <$> mapM (flatten a_fresh) ss

    InitAccumulator acc ss
     -> flatX a_fresh (accInit acc)
     $ \x'
     -> InitAccumulator (acc { accInit = x' }) <$> flatten a_fresh ss

    Read n m vt ss
     -> Read n m vt <$> flatten a_fresh ss

    Write n x
     -> flatX a_fresh x
     $ \x'
     -> return $ Write n x'

    Output n t xts
     | xs <- fmap fst xts
     , ts <- fmap snd xts
     -> flatXS a_fresh xs []
     $ \xs'
     -> return $ Output n t (List.zip xs' ts)

    KeepFactInHistory
     -> return $ KeepFactInHistory

    LoadResumable n t
     -> return $ LoadResumable n t
    SaveResumable n t
     -> return $ SaveResumable n t


flatXS  :: (Ord n, Pretty n)
        => a
        -> [Exp a n Core.Prim]
        -> [Exp a n Flat.Prim]
        -> ([Exp a n Flat.Prim] -> FlatM a n)
        -> FlatM a n

flatXS _       []     ys stm = stm ys
flatXS a_fresh (x:xs) ys stm
 = flatX a_fresh x
 $ \x'
 -> flatXS a_fresh xs (ys <> [x']) stm


-- | Flatten an expression, wrapping the statement with any lets or loops or other bindings
-- The statement function takes the new expression.
flatX   :: (Ord n, Pretty n)
        => a
        -> Exp a n Core.Prim
        -> (Exp a n Flat.Prim -> FlatM a n)
        -> FlatM a n

flatX a_fresh xx stm
 = convX
 where
  -- Do a bit of simplification.
  -- Betas must be converted to lets even if they are not simple values:
  -- Unapplied lambdas aren't really able to be converted, since
  -- we're lifting some expressions to statements, and there is no statement-level
  -- lambda, only expression-level lambda.
  x' = beta isSimpleValue
     $ betaToLets a_fresh xx

  -- Annotation plumbing
  flatX'    = flatX    a_fresh
  makeApps' = makeApps a_fresh
  xVar      = XVar     a_fresh
  xPrim     = XPrim    a_fresh
  xValue    = XValue   a_fresh
  xApp      = XApp     a_fresh

  -- Convert the simplified expression.
  convX
   = case x' of
      -- If it doesn't do anything interesting, we can just call the statement
      -- with the original expression
      XVar a n
       -> stm $ XVar a n
      XValue a vt bv
       -> stm $ XValue a vt bv

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

      XPrim _ p
       -> flatPrim p []

      -- Unapplied lambda: this should not happen for a well-typed program
      XLam{}
       -> lift $ Left $ FlattenErrorBareLambda x'


      -- Convert expression lets into statement lets
      XLet _ n p q
       -> flatX' p
       $ \p'
       -> Let n p' <$> flatX' q stm


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
       -> flatX' key
       $ \key'
       -> flatX' map
       $ \map'
       -> do n'lookup    <- fresh
             n'key       <- fresh
             n'val       <- fresh
             n'res       <- fresh
             n'map       <- fresh

             let t'       = MapT tk tv
                 acc'res  = Accumulator n'res  (MapT tk tv) map'


             let write x  = return $ Write n'res (fpMapUpd tk tv `makeApps'` [map', xVar n'key, x])


             x'update    <- flatX' (upd `xApp` xVar n'val) write
             x'insert    <- flatX' ins write

             let iffy     = Let n'key    key'
                          $ Let n'lookup (fpMapGet tk tv `makeApps'` [map', xVar n'key])
                          $ If (fpIsSome tv `xApp` xVar n'lookup)
                               (Let n'val (fpOptionGet tv `xApp` xVar n'lookup) x'update)
                               x'insert

             stm'        <- stm $ xVar n'map

             return $ InitAccumulator acc'res
                    (iffy <> Read n'map n'res t' stm')

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tk tv tv')
       | [upd, map]   <- xs
       -> flatX' map
       $ \map'
       -> do n'map  <- fresh

             let t' = MapT tk tv'
             loop   <- forI (fpMapLen tk tv `xApp` map')        $ \iter
                    -> slet (fpMapIx  tk tv `makeApps'` [map',iter])   $ \elm
                    -> slet (proj False tk tv elm)              $ \key
                    -> slet (proj True  tk tv elm)              $ \val
                    -> flatX' (upd `xApp` val)                  $ \val'
                    -> return
                     $ Read n'map n'map t'
                     $ Write n'map (fpMapUpd tk tv `makeApps'` [xVar n'map, key, val'])

             stm'   <- stm $ xVar n'map

             return  $ InitAccumulator (Accumulator n'map t' $ xValue t' $ VMap Map.empty)
                     ( loop <> Read n'map n'map t' stm')

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map over array: create new empty array, for each element, etc
      Core.PrimArray (Core.PrimArrayMap ta tb)
       | [upd, arr]   <- xs
       -> flatX' arr
       $ \arr'
       -> do    accN <- fresh
                stm' <- stm (xVar accN)

                let arrT = ArrayT tb

                loop <- forI (fpArrLen ta `xApp` arr')                 $ \iter
                     -> fmap    (Read accN accN arrT)                  $
                        slet    (fpArrIx ta `makeApps'` [arr', iter])  $ \elm
                     -> flatX'  (upd `xApp` elm)                    $ \elm'
                     -> slet    (fpArrUpd tb `makeApps'` [xVar accN, iter, elm']) $ \arr''
                     -> return  (Write accN arr'')

                return $ InitAccumulator
                            (Accumulator accN arrT (fpArrNew tb `xApp` (fpArrLen ta `xApp` arr')))
                            (loop <> Read accN accN arrT stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      Core.PrimLatest (Core.PrimLatestMake t)
       | [i] <- xs
       -> flatX' i
       $  \i'
       -> do   tmpN      <- fresh
               let fpBuf  = xPrim (Flat.PrimBuf $ Flat.PrimBufMake t)
               stm'      <- stm (xVar tmpN)

               return
                 $ Let tmpN (fpBuf `xApp` i') stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      Core.PrimLatest (Core.PrimLatestPush t)
       | [buf, e]    <- xs
       -> flatX' e
       $  \e'
       -> flatX' buf
       $  \buf'
       -> do   tmpN      <- fresh
               let fpPush = xPrim (Flat.PrimBuf $ Flat.PrimBufPush t) `xApp` buf' `xApp` e'
               stm'      <- stm (xVar tmpN)
               return
                 $ Let tmpN fpPush stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      Core.PrimLatest (Core.PrimLatestRead t)
       | [buf] <- xs
       -> flatX' buf
       $  \buf'
       -> do   tmpN       <- fresh
               let fpRead  = xPrim (Flat.PrimBuf $ Flat.PrimBufRead t) `xApp` buf'
               stm'       <- stm (xVar tmpN)
               return
                 $ Let tmpN fpRead stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


  -- Convert arguments to a simple primitive.
  -- conv is what we've already converted
  primApps p [] conv
   = stm
   $ makeApps' (xPrim p)
   $ reverse conv

  primApps p (a:as) conv
   = flatX' a
   $ \a'
   -> primApps p as (a' : conv)

  -- Create a let binding with a fresh name
  slet x ss
   = do n  <- fresh
        Let n x <$> ss (xVar n)

  -- For loop with fresh name for iterator
  forI to ss
   = do n  <- fresh
        ForeachInts n (xValue IntT (VInt 0)) to <$> ss (xVar n)


  -- Handle primitive folds
  --
  -- Bool is just an if
  flatFold Core.PrimFoldBool valT [then_, else_, pred]
   = flatX' pred
   $ \pred'
   -> do -- Fresh name for accumulator and result.
         -- We can use same name for acc & result variables because accumulators and variables are in different scopes
         acc <- fresh
         -- Compute the rest of the computation, assuming we've stored result in variable named acc
         stm' <- stm (xVar acc)
         sthen <- flatX' then_ $ (return . Write acc)
         selse <- flatX' else_ $ (return . Write acc)
         -- Perform if and write result
         let if_ =  If pred' sthen selse
         -- After if, read back result from accumulator and then go do the rest of the statements
         let read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Array fold becomes a loop
  flatFold (Core.PrimFoldArray telem) valT [k, z, arr]
   = do accN <- fresh
        stm' <- stm (xVar accN)

        -- Loop body updates accumulator with k function
        loop <-  flatX' arr                                       $ \arr'
             ->  forI   (fpArrLen telem `xApp` arr')              $ \iter
             ->  fmap   (Read accN accN valT)                     $
                 slet   (fpArrIx  telem `makeApps'` [arr', iter]) $ \elm
             ->  flatX' (makeApps' k [xVar accN, elm])            $ \x
             ->  return (Write accN x)

        -- Initialise accumulator with value z, execute loop, read from accumulator
        flatX' z $ \z' ->
            return (InitAccumulator (Accumulator accN valT z')
                   (loop <> Read accN accN valT stm'))


  -- Fold over map. Very similar to above
  flatFold (Core.PrimFoldMap tk tv) valT [k, z, map]
   =  flatX' map
   $ \map'
   -> flatX' z
   $ \z'
   -> do n'acc  <- fresh

         loop   <- forI (fpMapLen tk tv `xApp` map')        $ \iter
                -> slet (fpMapIx  tk tv `makeApps'` [map',iter])   $ \elm
                -> slet (proj False tk tv elm)              $ \key
                -> slet (proj True  tk tv elm)              $ \val
                -> fmap (Read n'acc n'acc valT)             $
                   flatX' (k `makeApps'` [xVar n'acc, key, val]) $ \acc'
                -> return
                 $ Write n'acc acc'

         stm'   <- stm $ xVar n'acc

         return  $ InitAccumulator (Accumulator n'acc valT z')
                 ( loop <> Read n'acc n'acc valT stm')



  -- Fold over an option is just "maybe" combinator.
  flatFold (Core.PrimFoldOption ta) valT [xsome, xnone, opt]
   = flatX' opt
   $ \opt'
   -> do
      acc  <- fresh
      stm' <- stm (xVar acc)
      tmp  <- fresh
      ssome <- flatX' (xsome `xApp` (xVar tmp)) $ (return . Write acc)
      snone <- flatX' xnone $ (return . Write acc)
      let if_   = If (fpIsSome ta `xApp` opt') (Let tmp (fpOptionGet ta `xApp` opt') ssome) snone
          -- After if, read back result from accumulator and then go do the rest of the statements
          read_ = Read acc acc valT stm'
      return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Fold over an either
  flatFold (Core.PrimFoldSum ta tb) valT [xleft, xright, scrut]
   = let fpIsRight   = xPrim (Flat.PrimProject  (Flat.PrimProjectSumIsRight ta tb))
         fpLeft      = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetLeft  ta tb))
         fpRight     = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetRight ta tb))
     in  flatX' scrut
      $ \scrut'
      -> do
         acc  <- fresh
         stm' <- stm (xVar acc)
         tmp  <- fresh
         tmp' <- fresh
         sleft   <- flatX' (xleft  `xApp` (xVar tmp )) $ (return . Write acc)
         sright  <- flatX' (xright `xApp` (xVar tmp')) $ (return . Write acc)

         let if_   = If (fpIsRight `xApp` scrut') (Let tmp' (fpRight `xApp` scrut') sright) (Let tmp (fpLeft `xApp` scrut') sleft)
           -- After if, read back result from accumulator and then go do the rest of the statements
             read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))


  -- None of the above cases apply, so must be bad arguments
  flatFold pf rt xs
   = lift $ Left $ FlattenErrorPrimBadArgs (Core.PrimFold pf rt) xs

  -- Create a fst# or snd#
  proj t ta tb e
   = let pm | not t
            = Min.PrimPairFst ta tb
            | otherwise
            = Min.PrimPairSnd ta tb
     in (xPrim $ Flat.PrimMinimal $ Min.PrimPair $ pm) `xApp` e


  fpArrLen t = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength t)
  fpArrIx  t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   t)
  fpArrNew t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayCreate  t)
  fpArrUpd t = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateArrayPut     t)

  fpMapUpd k v = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateMapPut     k v)
  fpMapLen k v = xPrim (Flat.PrimProject $ Flat.PrimProjectMapLength k v)
  fpMapGet k v = xPrim (Flat.PrimProject $ Flat.PrimProjectMapLookup k v)
  fpMapIx  k v = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeMapIndex   k v)

  fpIsSome    t = xPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome t))
  fpOptionGet t = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet t))

