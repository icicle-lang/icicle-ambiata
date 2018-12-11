-- | Converting Source to Core.
--
-- This is all very rough around the edges at the moment.
-- Some fundamental things are missing, like dealing with optional values:
-- the "newest v" aggregate returns a "Maybe v", but we actually want to implicitly
-- unbox that.
-- So, for example,
--
-- > let x = newest value ~> x * 5
--
-- here, "x" should be bound to a simple "Int", but the outer query should return
-- a "Maybe Int".
--
-- However, some things like filtering, grouping, latest and simple aggregates do work.
--
-- (This is why there are no property tests yet.)
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
module Icicle.Source.ToCore.ToCore (
    convertQueryTop
  , convertQuery
  ) where

import                  Data.Monoid ((<>))

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.ToCore.Exp
import                  Icicle.Source.ToCore.Fold
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Exp.Simp.Beta    as Beta
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base

import                  Icicle.Data.Name

import                  Icicle.Dictionary.Data

import                  P hiding ((<>))

import                  Control.Monad.Morph
import                  Control.Monad.Trans.State.Lazy
import                  Data.List (zip, unzip)

import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


-- | Convert a top-level Query to Core.
--
-- The input query is annotated with its type information at every node.
-- This is essential for both putting type annotations on the explicitly typed core,
-- and also because the universe annotations determine where computations are done:
--
-- "Pure" computations become Pre-computations in Core,
-- since they must not use on any stream inputs;
--
-- "Elem" computations must be worker functions on filters, maps, and so on.
--
-- "AggU" or "Group" computations can be reductions on streams, or postcomputations.
--
convertQueryTop :: (Hashable n, Eq n)
                => Features () n (InputKey (Annot a n) n)
                -> QueryTop (Annot a n) n
                -> FreshT n (Either (ConvertError a n)) (C.Program () n)
convertQueryTop feats qt
 = do   inp         <- fresh
        facttime    <- fresh
        now         <- fresh
        maxMapSize  <- fresh

        -- Lookup the fact that this query refers to.
        FeatureConcrete key ty fs
          <- lift
           $ maybeToRight (ConvertErrorNoSuchFeature (queryInput qt))
           $ lookupInputId (queryInput qt) (featuresConcretes feats)

        -- Extract the type of the input stream, pair with the fact time.
        inpTy <- case valTypeOfType ty of
                  Nothing -> lift $ Left $ ConvertErrorCannotConvertType (annAnnot $ annotOfQuery $ query qt) ty
                  Just t' -> return t'
        let inpTy'dated = T.PairT inpTy T.TimeT

        let convState = ConvertState inp inpTy'dated facttime now maxMapSize fs Map.empty
        let env       = Map.insert facttime (T.funOfVal   T.TimeT)
                      $ Map.insert inp      (T.funOfVal $ T.PairT inpTy T.TimeT) Map.empty

        -- Convert the query body.
        (bs, ret) <- flip evalStateT convState
                   $ do maybe (return ()) (flip convertFreshenAddAs now) $ featureNow feats
                        convertQuery (query qt) >>= convertKey env key

        return (programOfBinds (queryName qt) inpTy inp facttime now maxMapSize bs () ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
--
convertQuery :: (Hashable n, Eq n)
             => Query (Annot a n) n
             -> ConvertM a n (CoreBinds () n, Name n)
convertQuery q
 = convertContext
 $ case contexts q of
    -- There are no queries left, so deal with simple aggregates and nested queries.
    []
     -> convertReduce (final q)

    -- Converting filters is probably the simplest conversion.
    --
    -- We create a fresh name for the "filter" binding we're creating,
    -- as well as a fresh name for the lambda variable for the predicate.
    --
    -- Then we convert the filter expression into a predicate, and wrap it in the lambda.
    --
    -- We convert the rest of the query and pass through the fresh filter binding's name,
    -- as that is what the data is operating on.
    (Filter _ e : _)
     -> do  e'      <- convertExp e
            (bs, b) <- convertQuery q'
            let bs' = filt e' (streams bs) <> bs { streams = [] }
            return (bs', b)


    -- Windowing in Core is a standard filter, with precomputations for
    -- the edges of the windows.
    (Windowed _ newerThan olderThan : _)
     -> do  (bs, b)   <- convertQuery q'
            now       <- convertDateName
            time      <- convertFactTimeName
            leftEdge  <- lift fresh
            rightEdge <- lift fresh

            -- Bind the computations of the window
            -- edges as a pre-computations.
            let
              -- All widows have a left edge
              leftPrecomp
                = pre leftEdge $ windowEdge (CE.xVar now) newerThan
              leftComparison
                = CE.xVar time >=~ CE.xVar leftEdge

              -- If olderThan is set, we need to create
              -- the right edge of the window as well;
              -- otherwise, leave the comparison and
              -- precomputations as just the left edge.
              (bothPre, bothComparison)
                = case olderThan of
                    Just olderThan' ->
                      let
                        rightPrecomp =
                          pre rightEdge $ windowEdge (CE.xVar now) olderThan'
                        rightComparison =
                          CE.xVar rightEdge >=~ CE.xVar time
                      in
                        (leftPrecomp <> rightPrecomp, leftComparison CE.&&~ rightComparison)

                    Nothing ->
                      (leftPrecomp, leftComparison)

              -- Create the filtered program by filtering the downstream
              -- streams. We leave the precomputations and postcomputations
              -- of `bs` alone, and combine them with the new program.
              filteredProgram =
                filt bothComparison (streams bs) <> bs { streams = [] }

            return (bothPre <> filteredProgram, b)
          where
            windowEdge now (Days   d) = CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays) CE.@~ now CE.@~ CE.constI d
            windowEdge now (Weeks  w) = CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays) CE.@~ now CE.@~ CE.constI (w * 7)
            windowEdge now (Months m) = CE.xPrim (C.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusMonths) CE.@~ now CE.@~ CE.constI m

            (>=~) :: C.Exp () n -> C.Exp () n -> C.Exp () n
            (>=~) = CE.prim2 (C.PrimMinimal $ Min.PrimRelation Min.PrimRelationGe T.TimeT)
            infix 4 >=~

    (Latest _ _ : _)
     -> convertAsFold

    (GroupBy _ _ : _)
     -> convertAsFold

    -- Convert a group fold using a Map. Very similar to Group By, with an additional
    -- postcomputation.
    --
    -- The group itself constructs the Map and the group fold perform its aggregate
    -- on the Map.
    --
    (GroupFold (Annot { annAnnot = ann }) k v e : _ )
     -> do  (tk, tv) <- getGroupFoldType ann e

            n'   <- lift fresh
            nacc <- lift fresh

            -- Convert the inner group into a stream fold that produces a map.
            (bs, nm) <- convertReduce e

            -- The key and value will be available after the fold
            k' <- convertFreshenAdd k
            v' <- convertFreshenAdd v

            -- Convert the rest of the query into a map fold.
            res      <- convertFold q'
            let tacc  = typeFold res

            -- Perform the map fold.
            let p = post n'
                  $ beta
                  ( mapExtract res CE.@~
                  ( CE.xPrim
                      (C.PrimFold (C.PrimFoldMap tk tv) tacc)
                    CE.@~ ( CE.xLam nacc tacc
                          $ CE.xLam k'   tk
                          $ CE.xLam v'   tv
                              (foldKons res CE.@~ CE.xVar nacc))
                    CE.@~ foldZero res
                    CE.@~ CE.xVar nm))

            return (bs <> p, n')

    (Distinct _ _ : _)
     -> convertAsFold

    (Let _ b def : _)
     -> case getTemporalityOrPure $ annResult $ annotOfExp def of
         TemporalityElement
          -> do t' <- convertValType' $ annResult $ annotOfExp def
                (inpstream, inpty) <- convertInput
                let inpty' = T.PairT t' inpty

                e'      <- convertExp def

                let xfst = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t' inpty
                let xsnd = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t' inpty

                convertModifyFeaturesMap (Map.insert b (FeatureVariable (annResult $ annotOfExp def) xfst False) . Map.map (\fv -> fv { featureVariableExp = featureVariableExp fv . xsnd })) b

                let pairC l r
                     = (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t' inpty)
                         CE.@~ l CE.@~ r
                let pair = pairC e' (CE.xVar inpstream)
                let defT ty = CE.xValue ty (T.defaultOfType ty)
                let pairD= pairC (defT t') (defT inpty)


                n'r     <- lift fresh
                let bs   = sfold n'r inpty' pairD pair
                (bs', n'') <- convertWithInput n'r inpty' $ convertQuery q'

                return (bs <> bs', n'')

         TemporalityPure
          -> do e'      <- convertExp def
                b'      <- convertFreshenAdd b
                let bs   = pre b' e'
                (bs', n') <- convertQuery q'
                return (bs <> bs', n')

         TemporalityAggregate
          -> do (bs,n')      <- convertReduce    def
                b'           <- convertFreshenAdd b
                (bs',n'')    <- convertQuery q'
                return (bs <> post b' (CE.xVar n') <> bs', n'')

         _
          -> convertError $ ConvertErrorGroupByHasNonGroupResult (annAnnot $ annotOfExp def) (annResult $ annotOfExp def)


    -- Converting fold1s.
    (LetFold (Annot { annAnnot = ann }) Fold{ foldType = FoldTypeFoldl1 } : _)
     -> convertError $ ConvertErrorImpossibleFold1 ann

    -- In comparison, normal folds are quite easy.
    --
    -- > let fold summ ~> 0 : summ + value ~> ...
    -- >
    -- > =====>
    -- >
    -- > Stream.fold
    -- > (\a : Int. \v : Value. a + v)
    -- > 0
    -- > stream
    --
    (LetFold _ f@Fold{ foldType = FoldTypeFoldl } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f

            z   <- convertExp (foldInit f)
            -- Current accumulator is only available in worker
            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            n'a <- convertFreshenAdd $ foldBind f
            k   <- convertExp (foldWork f)

            -- Bind the fold to the original name
            let bs = sfold n'a tU z k

            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')

 where
  -- The remaining query after the current context is removed
  q' = q { contexts = drop 1 $ contexts q }

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets ()
       . Beta.beta

  convertValType' = convertValType (annAnnot $ annotOfQuery q)

  getGroupFoldType  = groupFoldType convertValType'

  -- When the convertFold and convertQuery are exactly the same, just use convertFold instead.
  convertAsFold = do
    res        <- convertFold q
    n'red      <- lift fresh
    n'ret      <- lift fresh

    let k'      = beta
                ( foldKons res CE.@~ CE.xVar n'red)

    let b'red   = sfold  n'red (typeFold res) (foldZero res) k'
    let b'ret   = post n'ret $ beta (mapExtract res CE.@~ CE.xVar n'red)

    return (b'red <> b'ret, n'ret)


-- | Convert an Aggregate computation at the end of a query.
-- This should be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
-- If not, it must be pure, so we can just bind it as a precomputation.
--
convertReduce :: (Hashable n, Eq n)
              => Exp (Annot a n) n
              -> ConvertM a n (CoreBinds () n, Name n)
convertReduce xx
 -- If it is Pure, just bind it as a precomputation
 | TemporalityPure <- getTemporalityOrPure $ annResult $ annotOfExp xx
 = do   x' <- convertExp xx
        nm <- lift fresh
        return (pre nm x', nm)

 | Just (p, Annot { annResult = ty }, args) <- takePrimApps xx
 -- For any primitives:
 --   recurse into its arguments and get bindings for them
 --   apply those bindings to the primitive, as a postcomputation
 --
 -- If the binding is Pure however, it must not rely on any aggregates,
 -- so it might as well be a precomputation.
 = do   (bs,nms) <- unzip <$> mapM convertReduce args
        let tys  = fmap (annResult . annotOfExp) args
        let xs   = fmap  CE.xVar           nms
        x' <- convertPrim p (annAnnot $ annotOfExp xx) ty (xs `zip` tys)

        nm  <- lift fresh

        let bs'  = mconcat bs
        let b''  | TemporalityPure <- getTemporalityOrPure ty
                 = pre nm x'
                 | otherwise
                 = post nm x'

        return (bs' <> b'', nm)


 -- Convert a nested query
 -- Any lets, folds etc bound in here will go out of scope at the end.
 -- So we revert the state at the end, clearing any bindings,
 -- rolling back to the old input type, etc.
 | Nested _ q   <- xx
 = do   o <- get
        r <- convertQuery q
        put o
        return r
 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var (Annot { annAnnot = ann }) v      <- xx
 = (,) mempty <$> convertFreshenLookup ann v


 | Case (Annot { annAnnot = ann, annResult = retty }) scrut patalts <- xx
 = do   scrut' <- convertReduce scrut

        -- Because the alternatives can contain more folds and stuff, we need to
        -- use convertReduce on them.
        -- However, because the pattern variables must be Aggregates, we know that
        -- any pattern variables will only be mentioned in the postcomputations.
        -- Therefore we need to pull out the postcomputations into a let,
        -- and stick them inside the new case alternative.
        --
        -- All the foldy bits of each alternative must be computed, because
        -- we won't know which ones will be needed until after they are run.
        let goPatAlt (p,alt)
                  = (,) <$> convertCaseFreshenPat p <*> convertReduce alt
        patalts' <- mapM goPatAlt patalts
        let pats' = fmap                 fst  patalts'
            alts' = fmap (pullPosts ())       patalts'

        let bs' = fst scrut' <> mconcat (fmap fst alts')

        let sX  = CE.xVar $ snd scrut'
        let aXs = fmap snd alts'

        scrutT <- convertValType ann $ annResult $ annotOfExp scrut
        resT   <- convertValType ann $ retty

        x'     <- convertCase xx sX (pats' `zip` aXs) scrutT resT
        nm     <- lift fresh

        let b'  | TemporalityPure <- getTemporalityOrPure retty
                = pre nm x'
                | otherwise
                = post nm x'

        return (bs' <> b', nm)

 -- It's not a variable or a nested query,
 -- so it must be an application of a non-primitive
 | otherwise
 = convertError $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot $ annotOfExp xx) xx

-- | Incorporate the refutation fact key into the query.
--
-- @
--   INIT:
--     (none, <fold_init>)
--   KONS:
--     new_key = nub_exp value
--     fold_bool
--       (old_key, acc)
--       (new_key, <fold_kons>)
--       (some new_key == old_key)
-- @
--
convertKey :: (Hashable n, Eq n)
           => T.Env n T.Type
           -> InputKey (Annot a n) n
           -> (CoreBinds () n, Name n)
           -> ConvertM a n (CoreBinds () n, Name n)
convertKey _   (InputKey Nothing)  bs          = return bs
convertKey env (InputKey (Just k)) (core, ret) = do
  -- Convert the key expression to Core.
  k'        <- convertExp k

  -- Synthesise a type for the key expression.
  t'k       <- lift . lift
             . first  (ConvertErrorCannotCheckKey (annAnnot (annotOfExp k)) k')
             . second (T.functionReturns)
             $ CE.typeExp C.coreFragmentWorkerFun env k'
  let t'key  = T.OptionT t'k

  let pairX t u = CE.xPrim . C.PrimMinimal . Min.PrimConst    $ Min.PrimConstPair  t u
  let fstX  t u = CE.xPrim . C.PrimMinimal . Min.PrimPair     $ Min.PrimPairFst    t u
  let sndX  t u = CE.xPrim . C.PrimMinimal . Min.PrimPair     $ Min.PrimPairSnd    t u
  let someX t   = CE.xPrim . C.PrimMinimal . Min.PrimConst    $ Min.PrimConstSome  t
  let eqX   t   = CE.xPrim . C.PrimMinimal $ Min.PrimRelation   Min.PrimRelationEq t

  let nubKons t'acc n'input xx = do
        n'acc'old <- lift fresh
        n'acc'new <- lift fresh
        n'key'old <- lift fresh
        n'key'new <- lift fresh
        n'unit    <- lift fresh

        xx' <- lift $ CE.subst1 () n'input (CE.xVar n'acc'old) xx
        let a  = CE.makeApps () (sndX  t'key t'acc) [CE.xVar n'input]
        let x' = CE.xLet n'key'old (CE.makeApps () (fstX  t'key t'acc) [CE.xVar n'input])
               $ CE.xLet n'acc'old a
               $ CE.xLet n'key'new (CE.makeApps () (someX t'k        ) [k'             ])
               $ CE.xLet n'acc'new xx'
               $ CE.makeApps ()
                   ( CE.xPrim (C.PrimFold C.PrimFoldBool (T.PairT t'key t'acc)) )
                   [ CE.xLam n'unit T.UnitT $ CE.xVar n'input
                   , CE.xLam n'unit T.UnitT $ CE.makeApps () (pairX t'key t'acc) [ CE.xVar n'key'new, CE.xVar n'acc'new ]
                   , CE.makeApps () (eqX   t'key      ) [ CE.xVar n'key'old, CE.xVar n'key'new ] ]

        pure ( x', Map.singleton n'input a )

  let nubInit t'acc xx
        = CE.makeApps () (pairX t'key t'acc) [ CE.xValue t'key VNone, xx ]

  -- Substitute the name of the stream input in the body of this stream and all downstreams,
  -- since the stream input should now refer to the key paired with the original input.
  let substStream subs (C.SFold f t ini kons)
        =   C.SFold f t
        <$> CE.subst () subs ini
        <*> CE.subst () subs kons
      substStream subs (C.SFilter f ss)
        =   C.SFilter
        <$> CE.subst () subs f
        <*> mapM (substStream subs) ss

  let substThen f (s : ss) = do
        (s', substs)   <- f [s]
        (ss', substs') <- f =<< lift (mapM (substStream substs) ss)
        pure (s' <> ss', substs <> substs')
      substThen _ []
        = pure ([], Map.empty)

  -- Prefix each streams with the nub expression.
  let nub (C.SFold n t ini kons : downstreams) = do
        (x', sub)    <- nubKons t n kons
        downstreams' <- lift $ mapM (substStream sub) downstreams
        (ss, subs)   <- nub downstreams'
        let s = C.SFold n (T.PairT t'key t) (nubInit t ini) x'
        pure (s : ss, sub <> subs)

      nub (C.SFilter x fs : downstreams) = do
        (fs', sub)    <- substThen nub fs
        downstreams'  <- lift $ mapM (substStream sub) downstreams
        (ss, subs)    <- nub downstreams'
        let s = C.SFilter x fs'
        pure (s : ss, sub <> subs)

      nub [] = pure ([], Map.empty)

  -- Remove the key at the end.
  let unkey ps [C.SFold n t'ret _ _]
        | n == ret = do
            n'ret <- lift fresh
            pure (ps <> [(n'ret, CE.makeApps () (sndX t'key t'ret) [CE.xVar n])], n'ret)
        | otherwise = pure (ps, ret)
      unkey p [C.SFilter _ ss] = unkey p ss
      unkey p (_ : ss)         = unkey p ss
      unkey p []               = pure (p, ret)

  (streams', subs)    <- hoist runFreshIdentity . nub
                       . streams $ core
  postcomps'          <- lift . runFreshIdentity
                       . mapM (\(n, x) -> (n,) <$> CE.subst () subs x)
                       . postcomps $ core
  (postcomps'', ret') <- unkey postcomps' (streams core)

  return ( core { streams = streams'
                , postcomps = postcomps'' }
         , ret' )
