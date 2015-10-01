{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Transform.ReifyPossibility (
    reifyPossibilityTransform
  ) where

import Icicle.Source.Query
import Icicle.Source.Type
import Icicle.Source.Transform.Base

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import              Data.Functor.Identity
import              Data.List (zip)
import qualified    Data.Map as Map

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
   = return ((), x)
  tranc _ c
   = case c of
      LetFold a f
       | FoldTypeFoldl1 <- foldType f
       -> do  nError <- fresh
              nValue <- fresh
              let b  = foldBind f
                  a' = a { annResult = canonT $ SumT ErrorT $ annResult a }

                  z' = con1 a' ConLeft $ ConError ErrorEmptyFold1

                  -- Will need to desugar after this
                  k' = Case a' b
                     [ ( PatCon ConLeft  [ PatCon $ ConError ErrorEmptyFold1 ]
                       , wrapRight $ foldZero f )
                     , ( PatCon ConLeft  [ PatVariable nError ]
                       , con1 a' ConLeft $ Var a' nError )
                     , ( PatCon ConRight [ PatVariable nValue ]
                       , wrapRight
                       $ wrapBareInput nValue
                       $ foldKons f ) ]

                  f' = f { foldType = FoldTypeFoldl
                         , foldZero = z'
                         , foldKons = k' }

              return ((), LetFold a' f')

      _
       -> return ((), c)

  con1 a c x = App a (Prim a (PrimCon c)) x

  wrapRight x
   | ann        <- annotOfExp x
   , t          <- annResult  ann
   , Possibly   <- getPossibilityOrDefinitely t
   = con1 (ann { annResult = canonT $ SumT ErrorT t) ConRight x
   | otherwise
   = x

  wrapBareInput n x
   = ...

