{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.ReifyPossibility (
    reifyPossibilityQT
  , reifyPossibilityQ
  , reifyPossibilityX
  , reifyPossibilityC
  ) where

import Icicle.Source.Query
import Icicle.Source.Type
import Icicle.Source.Transform.Base
import Icicle.Source.Transform.SubstX

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import Data.Functor.Identity
import qualified Data.Map as Map

reifyPossibilityQT
        :: Ord n
        => QueryTop (Annot a n) n
        -> Fresh n (QueryTop (Annot a n) n)
reifyPossibilityQT qt
 = do   q' <- reifyPossibilityQ (query qt)
        return $ qt { query = q' }

reifyPossibilityQ
        :: Ord n
        => Query (Annot a n) n
        -> Fresh n (Query (Annot a n) n)
reifyPossibilityQ q
 = do cs' <- mapM reifyPossibilityC $ contexts q
      x'  <- reifyPossibilityX $ final q
      return $ Query cs' x'

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
      -- TODO XXX this should perform case analysis on scrut if Possibly:
      -- case scrut of
      --  Left e -> Left e
      --  Right scrut' -> case scrut' -> alts ...
      Case a scrut alts
       -> do scrut' <-      reifyPossibilityX scrut
             alts'  <- mapM (\(p,xx) -> (,) p <$> reifyPossibilityX xx) alts
             return $ Case  (wrapAnnot a) (wrapRightIfAnnot a scrut')
                            (fmap (\(p,xx) -> (p, wrapRightIfAnnot a xx)) alts')

reifyPossibilityC
        :: Ord n
        => Context (Annot a n) n
        -> Fresh n (Context (Annot a n) n)
reifyPossibilityC c
   = case c of
      LetFold a f
       | FoldTypeFoldl1 <- foldType f
       -> do  nError <- fresh
              nValue <- fresh
              k      <- reifyPossibilityX $ foldWork f
              z      <- reifyPossibilityX $ foldInit f
              let b  = foldBind f
                  a'B = typeAnnot a BoolT
                  a'E = typeAnnot a ErrorT
                  a'D = definiteAnnot $ wrapAnnot a

                  vError = Var a'E nError
                  vValue = Var (definiteAnnot a) nValue

                  z' = con1 a'D ConLeft $ con0 a'E $ ConError ExceptFold1NoValue

                  eqError = App a'B
                                (App a'B (Prim a'B $ Op $ Relation Eq) (con0 a'E $ ConError ExceptFold1NoValue))
                                vError

                  k' = Case a'D (Var a'D b)
                     [ ( PatCon ConLeft  [ PatVariable nError ]
                       , Case a'D eqError
                            [ ( PatCon ConTrue []
                              , wrapRight $ z )
                            , ( PatCon ConFalse []
                              , con1 a'D ConLeft $ vError ) ] )
                     , ( PatCon ConRight [ PatVariable nValue ]
                       , wrapRight
                       $ substIntoIfDefinitely b vValue
                       $ k ) ]

                  f' = f { foldType = FoldTypeFoldl
                         , foldInit = z'
                         , foldWork = k' }

              return $ LetFold a'D f'

       | otherwise
       -> do  z' <- wrapRightIfAnnot (annotOfExp $ foldWork f) <$> reifyPossibilityX (foldInit f)
              k' <- wrapRightIfAnnot (annotOfExp $ foldInit f) <$> reifyPossibilityX (foldWork f)

              let f' = f { foldInit = z'
                         , foldWork = k' }

              return $ LetFold (wrapAnnot a) f'

      Windowed a w w'
       -> return $ Windowed  (wrapAnnot a) w w'
      Latest a i
       -> return $ Latest    (wrapAnnot a) i
      GroupBy a x
       -> GroupBy   (wrapAnnot a)     <$> reifyPossibilityX x
      Distinct a x
       -> Distinct  (wrapAnnot a)     <$> reifyPossibilityX x
      Filter a x
       -> Filter    (wrapAnnot a)     <$> reifyPossibilityX x
      Let a n x
       -> Let       (wrapAnnot a) n   <$> reifyPossibilityX x
      GroupFold a k v x
       -> GroupFold (wrapAnnot a) k v <$> reifyPossibilityX x


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

wrapAnnot :: Annot a n -> Annot a n
wrapAnnot ann
 | t                   <- annResult ann
 , PossibilityPossibly <- getPossibilityOrDefinitely t
 = ann { annResult = canonT $ SumT ErrorT t }
 | otherwise
 = ann

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
 = runIdentity
 $ transformX
   -- This unsafe subst transform is safe as long as the payload only mentions fresh variable names
   unsafeSubstTransform
 { transformState = Map.singleton var payload }
   into

