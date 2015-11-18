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
       -> do n'done      <- fresh
             n'map'k     <- fresh
             n'map'v     <- fresh
             n'map'sz    <- fresh
             let acc'done = Accumulator n'done  BoolT $ xValue BoolT $ VBool False
                 acc'map'k= Accumulator n'map'k (ArrayT tk) (fpMapKeys tk tv `xApp` map')
                 acc'map'v= Accumulator n'map'v (ArrayT tv) (fpMapVals tk tv `xApp` map')

                 x'done   = xVar n'done
                 x'map'k  = xVar n'map'k
                 x'map'v  = xVar n'map'v

                 read'k   = Read n'map'k n'map'k (ArrayT tk)
                 read'v   = Read n'map'v n'map'v (ArrayT tv)

                 sz       = xVar n'map'sz -- fpArrLen tk `xApp` x'map'k
                 eq       = xPrim $ Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationEq tk
                 get'k i  = fpArrIx  tk `makeApps'` [x'map'k, i]
                 get'v i  = fpArrIx  tv `makeApps'` [x'map'v, i]
                 put'v i x= fpArrUpd tv `makeApps'` [x'map'v, i, x]

                 upd' i   = slet (get'v i)
                          $ \v    -> flatX' (upd `xApp` v)
                          $ \u' -> return (Write n'map'v (put'v i u') <> Write n'done (xValue BoolT $ VBool True))

             loop1       <- forI sz  $ \i
                         ->  (read'k
                         <$> (read'v
                         <$> (If (eq `makeApps'` [get'k i, key'])
                               <$> upd' i
                               <*> return mempty)))

             loop2       <- pushArray tk n'map'k key'
             loop3       <- flatX' ins
                          $ pushArray tv n'map'v

             -- XX can't read with same name twice I think
             -- n'map'rr    <- fresh
             n'map'      <- fresh
             stm'        <- stm $ xVar n'map'

             let if_ins   = Read n'done n'done BoolT
                          $ If x'done mempty (loop2 <> loop3)

                 stm''    = read'k
                          $ read'v
                          $ Let n'map' (fpMapPack tk tv `makeApps'` [x'map'k, x'map'v])
                          $ stm'

                 ss       = InitAccumulator acc'done
                          $ InitAccumulator acc'map'k
                          $ InitAccumulator acc'map'v
                          $ read'k
                          $ Let n'map'sz (fpArrLen tk `xApp` x'map'k)
                          ( loop1 <> if_ins <> stm'' )

             return ss

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tk tv tv')
       | [upd, map]   <- xs
       -> flatX' map
       $ \map'
       -> do n'keys <- fresh
             n'vals <- fresh

             stm'   <- flatX'
                     ( xPrim (Core.PrimArray $ Core.PrimArrayMap tv tv')
                       `makeApps'` [upd, xVar n'vals] )
                     (\v -> stm (fpMapPack tk tv `makeApps'` [xVar n'keys, v]))

             return  $ Let n'vals (fpMapVals tk tv `xApp` map')
                     $ Let n'keys (fpMapKeys tk tv `xApp` map')
                     $ stm'

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


      Core.PrimLatest (Core.PrimLatestPush i t)
       | [buf, e]    <- xs
       -> flatX' e
       $  \e'
       -> flatX' buf
       $  \buf'
       -> do   tmpN      <- fresh
               let fpPush = xPrim (Flat.PrimBuf $ Flat.PrimBufPush i t) `xApp` buf' `xApp` e'
               stm'      <- stm (xVar tmpN)
               return
                 $ Let tmpN fpPush stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      Core.PrimLatest (Core.PrimLatestRead i t)
       | [buf] <- xs
       -> flatX' buf
       $  \buf'
       -> do   tmpN       <- fresh
               let fpRead  = xPrim (Flat.PrimBuf $ Flat.PrimBufRead i t) `xApp` buf'
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

  -- Update an accumulator
  update acc t x
   = do n'x <- fresh
        return $ Read n'x acc t $ Write acc $ x $ xVar n'x


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
  flatFold (Core.PrimFoldMap tk tv) valT [k, z, mmm]
   =  flatX' mmm
   $ \mmm'
   -> do n'keys  <- fresh
         n'vals  <- fresh
         n'zips  <- fresh

         n'kv <- fresh
         n'ac <- fresh
         let k'   = XLam a_fresh n'ac  valT
                  $ XLam a_fresh n'kv (PairT tk tv)
                  ( k `makeApps'` [ xVar n'ac
                                  , proj False tk tv $ xVar n'kv
                                  , proj True  tk tv $ xVar n'kv ])

         res     <- flatX'
                  ( xPrim (Core.PrimFold (Core.PrimFoldArray (PairT tk tv)) valT)
                     `makeApps'` [k', z, xVar n'zips] ) stm

         return   $ Let n'keys (fpMapKeys tk tv `xApp` mmm')
                  $ Let n'vals (fpMapVals tk tv `xApp` mmm')
                  $ Let n'zips (fpArrZip  tk tv `makeApps'` [xVar n'keys, xVar n'vals])
                  $ res



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
     in (xPrim $ Core.PrimMinimal $ Min.PrimPair $ pm) `xApp` e


  fpArrLen t = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength t)
  fpArrIx  t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   t)
  fpArrNew t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayCreate  t)
  fpArrUpd t = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateArrayPut     t)
  fpArrZip k v = xPrim (Flat.PrimArray $ Flat.PrimArrayZip           k v)


  fpMapPack k v = xPrim (Flat.PrimMap (Flat.PrimMapPack         k v))
  fpMapKeys k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackKeys   k v))
  fpMapVals k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackValues k v))

  fpIsSome    t = xPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome t))
  fpOptionGet t = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet t))

  pushArray t n'acc push
   = do let t'   = ArrayT t
            sz arr = fpArrLen t `xApp` arr
            put arr i x = fpArrUpd t `makeApps'` [arr, i, x]

        update n'acc t' (\arr -> put arr (sz arr) push)

