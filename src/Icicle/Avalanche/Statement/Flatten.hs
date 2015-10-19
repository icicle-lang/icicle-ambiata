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

    ForeachFacts n n' vt lo ss
     -> ForeachFacts n n' vt lo <$> flatten a_fresh ss

    Block ss
     -> Block <$> mapM (flatten a_fresh) ss

    InitAccumulator acc ss
     -> flatX a_fresh (accInit acc)
     $ \x'
     -> InitAccumulator (acc { accInit = x' }) <$> flatten a_fresh ss

    Read n m at vt ss
     -> Read n m at vt <$> flatten a_fresh ss

    Write n x
     -> flatX a_fresh x
     $ \x'
     -> return $ Write n x'

    Push n x
     -> flatX a_fresh x
     $ \x'
     -> return $ Push n x'

    Output n x
     -> flatX a_fresh x
     $ \x'
     -> return $ Output n x'

    KeepFactInHistory
     -> return $ KeepFactInHistory

    LoadResumable n t
     -> return $ LoadResumable n t
    SaveResumable n t
     -> return $ SaveResumable n t



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
       -> let fpLookup    = xPrim (Flat.PrimProject (Flat.PrimProjectMapLookup tk tv))
              fpIsSome    = xPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome tv))
              fpOptionGet = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet tv))
              fpUpdate    = xPrim (Flat.PrimUpdate (Flat.PrimUpdateMapPut tk tv))

              update val
                     =  slet    (fpOptionGet `xApp` val)                 $ \val'
                     -> flatX'  (upd `xApp` val')                        $ \upd'
                     -> slet    (makeApps' fpUpdate [map', key', upd'])  $ \map''
                     -> stm map''

              insert
                     =  flatX'  ins                                      $ \ins'
                     -> slet    (makeApps' fpUpdate [map', key', ins'])  $ \map''
                     -> stm map''

         in slet (makeApps' fpLookup [map', key'])                       $ \val
         ->  If (fpIsSome `xApp` val)
                <$> update val
                <*> insert

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tk tv tv')
       | [upd, map]   <- xs
       -> flatX' map
       $ \map'
       -> do    accN <- fresh
                let fpMapLen   = xPrim (Flat.PrimProject $ Flat.PrimProjectMapLength tk tv)
                let fpMapIx    = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeMapIndex   tk tv)
                let fpUpdate   = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateMapPut     tk tv')

                stm' <- stm (xVar accN)

                let mapT = MapT tk tv'
                    accT = Mutable

                loop <- forI (fpMapLen `xApp` map')                 $ \iter
                     -> fmap    (Read accN accN accT mapT)          $
                        slet    (fpMapIx `makeApps'` [map', iter])  $ \elm
                     -> slet    (proj False tk tv elm)              $ \efst
                     -> slet    (proj True  tk tv elm)              $ \esnd
                     -> flatX'  (upd `xApp` esnd)                   $ \esnd'
                     -> slet    (fpUpdate `makeApps'` [xVar accN, efst, esnd']) $ \map''
                     -> return  (Write accN map'')


                return $ InitAccumulator
                            (Accumulator accN accT mapT $ xValue mapT $ VMap Map.empty)
                            (loop <> Read accN accN accT mapT stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map over array: create new empty array, for each element, etc
      Core.PrimArray (Core.PrimArrayMap ta tb)
       | [upd, arr]   <- xs
       -> flatX' arr
       $ \arr'
       -> do    accN <- fresh
                let fpArrLen   = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength ta)
                let fpArrIx    = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   ta)
                let fpArrNew   = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayCreate  tb)
                let fpUpdate   = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateArrayPut     tb)

                stm' <- stm (xVar accN)

                let arrT = ArrayT tb
                    accT = Mutable

                loop <- forI (fpArrLen `xApp` arr')                 $ \iter
                     -> fmap    (Read accN accN accT arrT)          $
                        slet    (fpArrIx `makeApps'` [arr', iter])  $ \elm
                     -> flatX'  (upd `xApp` elm)                    $ \elm'
                     -> slet    (fpUpdate `makeApps'` [xVar accN, iter, elm']) $ \arr''
                     -> return  (Write accN arr'')

                return $ InitAccumulator
                            (Accumulator accN accT arrT (fpArrNew `xApp` (fpArrLen `xApp` arr')))
                            (loop <> Read accN accN accT arrT stm')


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
               return
                 $ Let tmpN fpPush mempty

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
  flatFold Core.PrimFoldBool _ [then_, else_, pred]
   -- XXX: we are using "stm" twice here,
   -- so duplicating branches.
   -- I don't think this is a biggie
   -- (yet)
   = flatX' pred
   $ \pred'
   -> If pred'
        <$> flatX' then_ stm
        <*> flatX' else_ stm

  -- Array fold becomes a loop
  flatFold (Core.PrimFoldArray telem) valT [k, z, arr]
   = do accN <- fresh
        stm' <- stm (xVar accN)

        let fpArrayLen = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength telem)
        let fpArrayIx  = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   telem)

        let accT = Mutable

        -- Loop body updates accumulator with k function
        loop <-  flatX' arr                                  $ \arr'
             ->  forI   (fpArrayLen `xApp` arr')             $ \iter
             ->  fmap   (Read accN accN accT valT)           $
                 slet   (fpArrayIx `makeApps'` [arr', iter]) $ \elm
             ->  flatX' (makeApps' k [xVar accN, elm])       $ \x
             ->  return (Write accN x)

        -- Initialise accumulator with value z, execute loop, read from accumulator
        flatX' z $ \z' ->
            return (InitAccumulator (Accumulator accN accT valT z')
                   (loop <> Read accN accN accT valT stm'))


  -- Fold over map. Very similar to above
  flatFold (Core.PrimFoldMap tk tv) valT [k, z, arr]
   = do accN <- fresh
        stm' <- stm (xVar accN)

        let fpMapLen   = xPrim (Flat.PrimProject $ Flat.PrimProjectMapLength tk tv)
        let fpMapIx    = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeMapIndex   tk tv)

        let accT = Mutable

        -- Loop is the same as for array, except we're grabbing the keys and values
        loop <- flatX' arr                                    $ \arr'
             -> forI    (fpMapLen `xApp` arr')                $ \iter
             -> fmap    (Read accN accN accT valT)            $
                slet    (fpMapIx `makeApps'` [arr', iter])    $ \elm
             -> slet    (proj False tk tv elm)                $ \efst
             -> slet    (proj True  tk tv elm)                $ \esnd
             -> flatX'  (makeApps' k [xVar accN, efst, esnd]) $ \x
             -> return  (Write accN x)

        flatX' z $ \z' ->
            return (InitAccumulator (Accumulator accN accT valT z')
                   (loop <> Read accN accN accT valT stm'))


  -- Fold over an option is just "maybe" combinator.
  flatFold (Core.PrimFoldOption ta) _ [xsome, xnone, opt]
   = let fpIsSome    = xPrim (Flat.PrimProject  (Flat.PrimProjectOptionIsSome ta))
         fpOptionGet = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeOptionGet     ta))
     in  flatX' opt
      $ \opt'
      -- If we have a value
      -> If (fpIsSome `xApp` opt')
         -- Rip the value out and apply it
         <$> slet (fpOptionGet `xApp` opt')
             (\val -> flatX' (xsome `xApp` val) stm)

         -- There's no value so return the none branch
         <*> flatX' xnone stm

  -- Fold over an either
  flatFold (Core.PrimFoldSum ta tb) _ [xleft, xright, scrut]
   = let fpIsLeft    = xPrim (Flat.PrimProject  (Flat.PrimProjectSumIsLeft  ta tb))
         fpLeft      = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetLeft  ta tb))
         fpRight     = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetRight ta tb))
     in  flatX' scrut
      $ \scrut'
      -- If we have a value
      -> If (fpIsLeft `xApp` scrut')
         -- Rip the left out and apply it
         <$> slet (fpLeft `xApp` scrut')
             (\val -> flatX' (xleft `xApp` val) stm)

         -- Take right
         <*> slet (fpRight `xApp` scrut')
             (\val -> flatX' (xright `xApp` val) stm)


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
