{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.ReifyPossibility (
    reifyPossibilityQT
  , reifyPossibilityQ
  , reifyPossibilityX
  ) where

import Icicle.Source.Query
import Icicle.Source.Type
import Icicle.Source.Transform.SubstX

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import qualified Data.Map as Map

reifyPossibilityQT
        :: Ord n
        => QueryTop (Annot a n) n
        -> Fresh n (QueryTop (Annot a n) n)
reifyPossibilityQT qt
 = do   q' <- reifyPossibilityQ (query qt)
        return $ qt { query = q' }

reifyPossibilityX
        :: Ord n
        => Exp (Annot a n) n
        -> Fresh n (Exp (Annot a n) n)
reifyPossibilityX x
   = case x of
      Var a n
       -> return $ Var (wrapAnnot a) n
      Nested a q
       -> Nested (wrapAnnot a) <$> reifyPossibilityQ q
      App a _ _
       -> do let (fun,args) = takeApps x
             fun'  <-      reifyPossibilityX fun
             args' <- mapM reifyPossibilityX args
             makeApps a fun' args' False
      Prim a p
       -> return $ Prim (wrapAnnot a) p
      -- If the scrutinee is possibly, we need to unwrap it before performing the case:
      -- > case scrut | alt -> ...
      -- ==>
      -- > case scrut
      -- > | Left e -> Left e
      -- > | Right scrut' -> case scrut' | alt -> ...
      Case a scrut alts
       | t <- annResult $ annotOfExp scrut
       , PossibilityPossibly <- getPossibilityOrDefinitely t
       -> do  nError <- fresh
              nValue <- fresh
              scrut' <- reifyPossibilityX scrut
              let a'  = wrapAnnot a
                  a'E = typeAnnot a ErrorT
                  a'D = definiteAnnot $ annotOfExp scrut

                  vError = Var a'E nError
                  vValue = Var a'D nValue

              alts'  <- mapM (\(p,xx) -> (,) p <$> (wrapRightIfAnnot a <$> reifyPossibilityX xx)) alts

              return $ Case (wrapAnnot a) scrut'
                            [ ( PatCon ConLeft  [ PatVariable nError ]
                              , con1 a' ConLeft $ vError )
                            , ( PatCon ConRight [ PatVariable nValue ]
                              , Case (wrapAnnot a) vValue alts' ) ]

       -- Scrutinee is definite
       | otherwise
       -> do  scrut' <-      reifyPossibilityX scrut
              -- If the return of the case is a Possibly, then at least one of the alternatives must be possibly.
              -- For the non-possibly alternatives, wrap them as a Right.
              alts'  <- mapM (\(p,xx) -> (,) p <$> (wrapRightIfAnnot a <$> reifyPossibilityX xx)) alts
              return $ Case  (wrapAnnot a) scrut' alts'


reifyPossibilityQ
        :: Ord n
        => Query (Annot a n) n
        -> Fresh n (Query (Annot a n) n)
reifyPossibilityQ (Query [] x)
 = Query [] <$> reifyPossibilityX x
reifyPossibilityQ (Query (c:cs) final_x)
 = case c of
    LetFold a f
     | FoldTypeFoldl1 <- foldType f
     -> do  nError <- fresh
            nValue <- fresh
            -- We need to give the fold a new name, because moving the "z" into the "k" means
            -- the binding is available under "z" now, which potentially shadows an existing binding.
            nBind  <- freshPrefix'      $ foldBind f

            k      <- reifyPossibilityX $ foldWork f
            z      <- reifyPossibilityX $ foldInit f
            let a'B = typeAnnot a BoolT
                a'E = typeAnnot a ErrorT
                a'D = aggAnnot $ wrapAnnotReally $ annotOfExp $ foldWork f

                vError = Var a'E nError
                vValue = Var (annotOfExp $ foldWork f) nValue
                vBind  = Var a'D nBind

                z' = con1 a'D ConLeft $ con0 a'E $ ConError ExceptFold1NoValue

                eqError = App a'B
                              (App a'B (Prim a'B $ Op $ Relation Eq) (con0 a'E $ ConError ExceptFold1NoValue))
                              vError

                k' = Case a'D vBind
                   [ ( PatCon ConLeft  [ PatVariable nError ]
                     , Case a'D eqError
                          [ ( PatCon ConTrue []
                            , wrapRight $ z )
                          , ( PatCon ConFalse []
                            , con1 a'D ConLeft $ vError ) ] )
                   , ( PatCon ConRight [ PatVariable nValue ]
                     , wrapRight
                     $ substIntoIfDefinitely nBind vValue
                     $ substInto (foldBind f) vBind
                     $ k ) ]

                f' = f { foldType = FoldTypeFoldl
                       , foldBind = nBind
                       , foldInit = z'
                       , foldWork = k' }

            rest' <- rest
            let rsub = unsafeSubstQ (Map.singleton (foldBind f) vBind) rest'
            return $ ins (LetFold (wrapAnnot a) f') rsub

     | otherwise
     -> do  z' <- wrapRightIfAnnot (annotOfExp $ foldWork f) <$> reifyPossibilityX (foldInit f)
            k' <- wrapRightIfAnnot (annotOfExp $ foldInit f) <$> reifyPossibilityX (foldWork f)

            let f' = f { foldInit = z'
                       , foldWork = k' }

            add $ LetFold (wrapAnnot a) f'

    Windowed a w w'
     -> add $ Windowed  (wrapAnnot a) w w'
    Latest a i
     -> add $ Latest    (wrapAnnot a) i
    GroupBy a x
     -> add' (GroupBy   (wrapAnnot a)     <$> reifyPossibilityX x)
    Distinct a x
     -> add' (Distinct  (wrapAnnot a)     <$> reifyPossibilityX x)
    Filter a x
     | preda               <- annotOfExp x
     , PossibilityPossibly <- getPossibilityOrDefinitely $ annResult preda
     -> do  x'     <- reifyPossibilityX x
            nError <- fresh
            nValue <- fresh
            let vValue  = Var preda nValue

                pred'   = Case preda x'
                            [ ( PatCon ConLeft  [PatVariable nError] , con0 preda ConTrue )
                            , ( PatCon ConRight [PatVariable nValue] , vValue ) ]
            rest'    <- rest
            return $ ins (Filter (wrapAnnot a) pred') (Query [] $ wrapRight $ Nested (annotOfQuery rest') rest')
     | otherwise
     -> add' (Filter    (wrapAnnot a)     <$> reifyPossibilityX x)
    Let a n x
     -> add' (Let       (wrapAnnot a) n   <$> reifyPossibilityX x)
    GroupFold a k v grp
     | grpa                <- annotOfExp grp
     , PossibilityPossibly <- getPossibilityOrDefinitely $ annResult grpa
     -> do  nError <- fresh
            nValue <- fresh
            grp'   <- reifyPossibilityX grp
            let a'  = wrapAnnot a
                a'E = typeAnnot a ErrorT

                vError = Var a'E nError
                vValue = Var grpa nValue

            rest'    <- rest
            -- The inner return must be an aggregate
            let a'R   = aggAnnot $ wrapAnnot $ annotOfQuery rest'
                ins'  = ins (GroupFold a'R k v vValue) rest'


            let xx = Case (wrapAnnot a) grp'
                          [ ( PatCon ConLeft  [ PatVariable nError ]
                            , con1 a' ConLeft $ vError )
                          , ( PatCon ConRight [ PatVariable nValue ]
                            , wrapRight $ Nested a'R ins' ) ]
            return (Query [] xx)

     | otherwise
     -> add' (GroupFold (wrapAnnot a) k v <$> reifyPossibilityX grp)

 where
  rest
   = reifyPossibilityQ (Query cs final_x)

  add ctx
   = add' (return ctx)
  add' ctx
   = ins <$> ctx <*> rest
  ins ctx (Query ctxs x)
   = Query (ctx:ctxs) x

-- XXX this is ignoring the possibility of functions that return differing modes.
-- This is true of all current primitives, and at this stage we can only have primitives.
makeApps
        :: Annot a n
        ->  Exp (Annot a n) n
        -> [Exp (Annot a n) n]
        -> Bool
        -> Fresh n (Exp (Annot a n) n)
makeApps _ fun [] doWrap
 = let funR = conRight fun
   in  if   doWrap
       then return funR
       else return fun

makeApps a fun (arg:rest) doWrap
 -- Check if the argument is a possibly.
 -- If so, we need to unwrap it before applying
 | arga                <- annotOfExp arg
 , PossibilityPossibly <- getPossibilityOrDefinitely $ annResult arga
 =  do  nError <- fresh
        nValue <- fresh
        let a'    = wrapAnnot a
            err   = con1 a' ConLeft $ Var (definiteAnnot a) nError

            -- Bare value. Note that this is now definite, but with same (bare) type
            bare  = Var (extractValueAnnot arga) nValue

        fun' <- makeApps a (App a fun bare) rest True

        let app'  = Case a' arg
                  [ ( PatCon ConLeft  [ PatVariable nError ]
                    , err )
                  , ( PatCon ConRight [ PatVariable nValue ]
                    , fun') ]

        return app'

 -- If argument is a definitely, just apply it as usual
 | otherwise
 =  makeApps a (App a fun arg) rest doWrap


con0 :: Annot a n -> Constructor -> Exp (Annot a n) n
con0 a c   =        Prim a (PrimCon c)

con1 :: Annot a n -> Constructor -> Exp (Annot a n) n -> Exp (Annot a n) n
con1 a c x = App a (Prim a (PrimCon c)) x

aggAnnot :: Annot a n -> Annot a n
aggAnnot ann
 = let t = annResult ann
   in  ann { annResult = canonT $ Temporality TemporalityAggregate t }

wrapAnnot :: Annot a n -> Annot a n
wrapAnnot ann
 | t                   <- annResult ann
 , PossibilityPossibly <- getPossibilityOrDefinitely t
 = wrapAnnotReally ann
 | otherwise
 = ann

wrapAnnotReally :: Annot a n -> Annot a n
wrapAnnotReally ann
 = let t = annResult ann
   in  ann { annResult = canonT $ Possibility PossibilityPossibly $ SumT ErrorT t }


extractValueAnnot :: Annot a n -> Annot a n
extractValueAnnot ann
 | (tmp, _, dat)  <- decomposeT $ annResult ann
 , SumT ErrorT tv <- dat
 , t'             <- recomposeT (tmp, Just PossibilityDefinitely, tv)
 = ann { annResult = t' }
 | otherwise
 = ann

definiteAnnot :: Annot a n -> Annot a n
definiteAnnot ann
 = ann { annResult = canonT $ Possibility PossibilityDefinitely $ annResult ann }

typeAnnot :: Annot a n -> Type n -> Annot a n
typeAnnot ann t
 = ann { annResult = t }


wrapRightIfAnnot :: Annot a n -> Exp (Annot a n) n -> Exp (Annot a n) n
wrapRightIfAnnot ann x
 | t                   <- annResult ann
 , PossibilityPossibly <- getPossibilityOrDefinitely t
 = wrapRight x
 | otherwise
 = x

wrapRight :: Exp (Annot a n) n -> Exp (Annot a n) n
wrapRight x
 | ann        <- annotOfExp x
 , t          <- annResult  ann
 , PossibilityDefinitely <- getPossibilityOrDefinitely t
 = conRight x
 | otherwise
 = x

conRight :: Exp (Annot a n) n -> Exp (Annot a n) n
conRight x
 = let ann = annotOfExp x
       t   = annResult  ann
   in con1 (ann { annResult = canonT $ SumT ErrorT t } ) ConRight x

substIntoIfDefinitely
        :: Ord n
        => Name n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
substIntoIfDefinitely var payload into
 | PossibilityDefinitely <- getPossibilityOrDefinitely $ annResult $ annotOfExp into
 = substInto var payload into
 | otherwise
 = into


substInto
        :: Ord n
        => Name n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
substInto var payload into
   -- This unsafe subst transform is safe as long as the payload only mentions fresh variable names
 = unsafeSubstX (Map.singleton var payload)
   into

