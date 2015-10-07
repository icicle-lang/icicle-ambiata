{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.ReifyPossibility (
    reifyPossibilityTransform
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

reifyPossibilityTransform
        :: Ord n
        => Transform (Fresh n) () (Annot a n) n
reifyPossibilityTransform
 = Transform
 { transformExp         = tranx
 , transformPat         = \_ p -> return ((), p)
 , transformContext     = tranc
 , transformState       = ()
 }
 where
  tranx _ x
   = case x of
      Var a n
       -> return ((), Var   (wrapAnnot a) n)
      Nested a q
       -> return ((), wrapRightIfAnnot a $ Nested (wrapAnnot a) q)
      App a _ _
       -> do let (fun,args) = takeApps x
             x' <- makeApps a fun args False
             return ((), x')
      Prim a p
       -> return ((), Prim  (wrapAnnot a) p)
      Case a scrut alts
       -> return ((), Case  (wrapAnnot a) (wrapRightIfAnnot a scrut)
                            (fmap (\(p,xx) -> (p, wrapRightIfAnnot a xx)) alts))

  tranc _ c
   = case c of
      LetFold a f
       | FoldTypeFoldl1 <- foldType f
       -> do  nError <- fresh
              nValue <- fresh
              let b  = foldBind f
                  a' = wrapAnnot a

                  z' = con1 a' ConLeft $ con0 (definiteAnnot a) $ ConError ExceptFold1NoValue

                  -- Will need to desugar after this
                  k' = Case a' (Var a' b)
                     [ ( PatCon ConLeft  [ PatCon (ConError ExceptFold1NoValue) [] ]
                       , wrapRight $ foldInit f )
                     , ( PatCon ConLeft  [ PatVariable nError ]
                       , con1 a' ConLeft $ Var (definiteAnnot a) nError )
                     , ( PatCon ConRight [ PatVariable nValue ]
                       , wrapRight
                       $ substIntoIfDefinitely b (Var (definiteAnnot a) nValue)
                       $ foldWork f ) ]

                  f' = f { foldType = FoldTypeFoldl
                         , foldInit = z'
                         , foldWork = k' }

              return ((), LetFold a' f')

       | otherwise
       -> do  let -- If the other part returns a possibly but this doesn't, wrap it
                  z' = wrapRightIfAnnot (annotOfExp $ foldWork f) $ foldInit f
                  k' = wrapRightIfAnnot (annotOfExp $ foldInit f) $ foldWork f

                  f' = f { foldInit = z'
                         , foldWork = k' }

              return ((), LetFold (wrapAnnot a) f')

      Windowed a w w'
       -> return ((), Windowed  (wrapAnnot a) w w')
      Latest a i
       -> return ((), Latest    (wrapAnnot a) i)
      GroupBy a x
       -> return ((), GroupBy   (wrapAnnot a)       x)
      Distinct a x
       -> return ((), Distinct  (wrapAnnot a)       x)
      Filter a x
       -> return ((), Filter    (wrapAnnot a)       x)
      Let a n x
       -> return ((), Let       (wrapAnnot a) n     x)
      GroupFold a k v x
       -> return ((), GroupFold (wrapAnnot a) k v   x)


  -- XXX this is ignoring the possibility of functions that return differing modes.
  -- This is true of all current primitives, and at this stage we can only have primitives.
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
          let a'    = definiteAnnot a
              arga' = wrapAnnot arga
              err   = con1 arga' ConLeft $ Var a' nError

              -- Bare value. Note that this is now definite, but with same (bare) type
              bare  = Var (definiteAnnot arga) nValue

          fun' <- makeApps a' (App a' fun bare) rest True

          let app'  = Case arga' arg
                    [ ( PatCon ConLeft  [ PatVariable nError ]
                      , err )
                    , ( PatCon ConRight [ PatVariable nValue ]
                      , fun') ]

          return app'

   -- If argument is a definitely, just apply it as usual
   | otherwise
   =  makeApps a (App a fun arg) rest doWrap


  con0 a c   =        Prim a (PrimCon c)
  con1 a c x = App a (Prim a (PrimCon c)) x

  wrapAnnot ann
   | t                   <- annResult ann
   , PossibilityPossibly <- getPossibilityOrDefinitely t
   = ann { annResult = canonT $ SumT ErrorT t }
   | otherwise
   = ann

  definiteAnnot ann
   = ann { annResult = canonT $ Possibility PossibilityDefinitely $ annResult ann }


  wrapRightIfAnnot ann x
   | t                   <- annResult ann
   , PossibilityPossibly <- getPossibilityOrDefinitely t
   = wrapRight x
   | otherwise
   = x

  wrapRight x
   | ann        <- annotOfExp x
   , t          <- annResult  ann
   , PossibilityDefinitely <- getPossibilityOrDefinitely t
   = conRight x
   | otherwise
   = x

  conRight x
   = let ann = annotOfExp x
         t   = annResult  ann
     in con1 (ann { annResult = canonT $ Possibility PossibilityPossibly $ SumT ErrorT t } ) ConRight x

  substIntoIfDefinitely var payload into
   | PossibilityDefinitely <- getPossibilityOrDefinitely $ annResult $ annotOfExp into
   = substInto var payload into
   | otherwise
   = into

  substInto var payload into
   = runIdentity
   $ transformX
     unsafeSubstTransform
   { transformState = Map.singleton var payload }
     into

