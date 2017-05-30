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
import           Data.Hashable (Hashable)


reifyPossibilityQT
        :: (Hashable n, Eq n)
        => QueryTop (Annot a n) n
        -> Fresh n (QueryTop (Annot a n) n)
reifyPossibilityQT qt
 = do   q' <- reifyPossibilityQ (query qt)
        return $ qt { query = q' }

reifyPossibilityX
        :: (Hashable n, Eq n)
        => (Annot a n -> Exp (Annot a n) n -> Exp (Annot a n) n)
        -> Exp (Annot a n) n
        -> Fresh n (Exp (Annot a n) n)
reifyPossibilityX wrap x
   = case x of
      Var a n
       -> return $ Var (wrapAnnot a) n
      Nested a q
       -> Nested (wrapAnnot a) <$> reifyPossibilityQ q

      -- If an explicit box is used, we do a case distinction on the argument
      App a _ _
       -- Box of a Possibly is a bit more complicated,
       -- because we need to unwrap it first
       | Just (p, _, [arg])     <- takePrimApps x
       , Fun (BuiltinData Box)  <- p
       , t                      <- annResult $ annotOfExp arg
       , PossibilityPossibly    <- getPossibilityOrDefinitely t
       -> do nValue     <- fresh
             let aValue  = definiteAnnot a
             let aError  = typeAnnot aValue ErrorT
             let vValue  = Var  aValue nValue
             let vError  = Prim aError (PrimCon (ConError ExceptTombstone))
             reifyPossibilityX wrapAsSum
               $ Case a arg
                   [ ( PatCon ConSome [PatVariable nValue], vValue)
                   , ( PatCon ConNone [],                   vError) ]

       -- Box of a definitely can just wrap in the left/right immediately
       | Just (p, _, [arg])   <- takePrimApps x
       , Fun (BuiltinData Box) <- p
       -> do arg'       <- reifyPossibilityX wrap arg
             nValue     <- fresh
             let a'  = wrapAnnot a
             let aValue  = definiteAnnot a
             let aError  = typeAnnot a' ErrorT
             let vValue  = Var  aValue nValue
             let vError  = Prim aError (PrimCon (ConError ExceptTombstone))
             return
               $ Case a' arg'
                   [ ( PatCon ConSome [PatVariable nValue], con1 a' ConRight $ vValue)
                   , ( PatCon ConNone [],                   con1 a' ConLeft $ vError ) ]


       | otherwise
       -> do let (fun,args) = takeApps x
             fun'  <-       reifyPossibilityX wrap fun
             args' <- mapM (reifyPossibilityX wrap) args
             makeApps a fun' args' False

      -- Primitives do not need their annotation to be wrapped:
      -- if they are Possiblies, their outer use will be wrapped in a Right.
      Prim a p
       -> return $ Prim a p

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
              scrut' <- reifyPossibilityX wrap scrut
              let a'  = wrapAnnot a
                  a'E = typeAnnot a ErrorT
                  a'D = definiteAnnot $ annotOfExp scrut

                  vError = Var a'E nError
                  vValue = Var a'D nValue

              alts'  <- mapM (\(p,xx) -> (,) p <$> (wrap a <$> reifyPossibilityX wrap xx)) alts

              return $ Case (wrapAnnot a) scrut'
                            [ ( PatCon ConLeft  [ PatVariable nError ]
                              , con1 a' ConLeft $ vError )
                            , ( PatCon ConRight [ PatVariable nValue ]
                              , Case (wrapAnnot a) vValue alts' ) ]

       -- Scrutinee is definite
       | otherwise
       -> do  scrut' <-      reifyPossibilityX wrap scrut
              -- If the return of the case is a Possibly, then at least one of the alternatives must be possibly.
              -- For the non-possibly alternatives, wrap them as a Right.
              alts'  <- mapM (\(p,xx) -> (,) p <$> (wrapRightIfAnnot a <$> reifyPossibilityX wrap xx)) alts
              return $ Case  (wrapAnnot a) scrut' alts'


reifyPossibilityQ
        :: (Hashable n, Eq n)
        => Query (Annot a n) n
        -> Fresh n (Query (Annot a n) n)
reifyPossibilityQ (Query [] x)
 = Query [] <$> reifyPossibilityX wrapRightIfAnnot x
reifyPossibilityQ (Query (c:cs) final_x)
 = case c of
    LetFold a f
     | FoldTypeFoldl1 <- foldType f
     -> do  nError <- fresh
            nValue <- fresh
            -- We need to give the fold a new name, because moving the "z" into the "k" means
            -- the binding is available under "z" now, which potentially shadows an existing binding.
            nBind  <- freshPrefixBase   $ nameBase $ foldBind f

            k      <- reifyPossibilityX wrapRightIfAnnot $ foldWork f
            z      <- reifyPossibilityX wrapRightIfAnnot $ foldInit f
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
             
            kS    <- substInto (foldBind f) vBind k
            kS'   <- substIntoIfDefinitely nBind vValue kS

            let k' = Case a'D vBind
                   [ ( PatCon ConLeft  [ PatVariable nError ]
                     , Case a'D eqError
                          [ ( PatCon ConTrue []
                            , wrapRight $ z )
                          , ( PatCon ConFalse []
                            , con1 a'D ConLeft $ vError ) ] )
                   , ( PatCon ConRight [ PatVariable nValue ]
                     , wrapRight kS') ]

                f' = f { foldType = FoldTypeFoldl
                       , foldBind = nBind
                       , foldInit = z'
                       , foldWork = k' }

            rest' <- rest
            rsub  <- substQ (Map.singleton (foldBind f) vBind) rest'
            return $ ins (LetFold (wrapAnnot a) f') rsub

     | otherwise
     -> do  z' <- wrapRightIfAnnot (annotOfExp $ foldWork f) <$> reifyPossibilityX wrapRightIfAnnot (foldInit f)
            k' <- wrapRightIfAnnot (annotOfExp $ foldInit f) <$> reifyPossibilityX wrapRightIfAnnot (foldWork f)

            let f' = f { foldInit = z'
                       , foldWork = k' }

            add $ LetFold (wrapAnnot a) f'

    Windowed a w w'
     -> add $ Windowed  (wrapAnnot a) w w'
    Latest a i
     -> add $ Latest    (wrapAnnot a) i
    GroupBy a x
     -> add' (GroupBy   (wrapAnnot a)     <$> reifyPossibilityX wrapRightIfAnnot x)
    Distinct a x
     -> add' (Distinct  (wrapAnnot a)     <$> reifyPossibilityX wrapRightIfAnnot x)
    Filter a x
     | preda               <- annotOfExp x
     , PossibilityPossibly <- getPossibilityOrDefinitely $ annResult preda
     -> do  x'     <- reifyPossibilityX wrapRightIfAnnot x
            nError <- fresh
            nValue <- fresh
            let vValue  = Var preda nValue

                pred'   = Case preda x'
                            [ ( PatCon ConLeft  [PatVariable nError] , con0 preda ConTrue )
                            , ( PatCon ConRight [PatVariable nValue] , vValue ) ]
            rest'    <- rest
            return $ ins (Filter (wrapAnnot a) pred') (Query [] $ wrapRight $ Nested (annotOfQuery rest') rest')
     | otherwise
     -> add' (Filter    (wrapAnnot a)     <$> reifyPossibilityX wrapRightIfAnnot x)
    Let a n x
     -> add' (Let       (wrapAnnot a) n   <$> reifyPossibilityX wrapRightIfAnnot x)
    GroupFold a k v grp
     | grpa                <- annotOfExp grp
     , PossibilityPossibly <- getPossibilityOrDefinitely $ annResult grpa
     -> do  nError <- fresh
            nValue <- fresh
            grp'   <- reifyPossibilityX wrapRightIfAnnot grp
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
     -> add' (GroupFold (wrapAnnot a) k v <$> reifyPossibilityX wrapRightIfAnnot grp)

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
        :: Hashable n
        => Annot a n
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

        -- Check if we're calling a primitive which already returns its result wrapped.
        -- If so, we do not need to perform rewrapping of the value.
        let doWrap'
              | Just (p, _, _) <- takePrimApps fun
              , primReturnsPossibly p
              = False
              | otherwise
              = True

        fun' <- makeApps a (App a fun bare) rest doWrap'

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
 -- Make sure we keep the temporality the same, because the conversion to Core
 -- uses temporality to decide where to put things
 = let (tmpq,posq,_) = decomposeT $ annResult ann
       t'            = recomposeT (tmpq, posq, t)
   in ann { annResult = t' }

wrapRightIfAnnot :: Annot a n -> Exp (Annot a n) n -> Exp (Annot a n) n
wrapRightIfAnnot ann x
 | t                   <- annResult ann
 , PossibilityPossibly <- getPossibilityOrDefinitely t
 = wrapRight x
 | otherwise
 = x

wrapRight :: Exp (Annot a n) n -> Exp (Annot a n) n
wrapRight x
 | ann <- annotOfExp x
 , t   <- annResult  ann
 , PossibilityDefinitely <- getPossibilityOrDefinitely t
 = conRight x
 | otherwise
 = x

conRight :: Exp (Annot a n) n -> Exp (Annot a n) n
conRight x
 = let ann = annotOfExp x
       t   = annResult  ann
   in con1 (ann { annResult = canonT $ SumT ErrorT t } ) ConRight x

wrapAsSum :: Annot a n -> Exp (Annot a n) n -> Exp (Annot a n) n
wrapAsSum v x
  = case decomposeT (annResult (annotOfExp x)) of
      (_,_,ErrorT) -> conLeft v x
      _            -> conRight x

conLeft :: Annot a n -> Exp (Annot a n) n -> Exp (Annot a n) n
conLeft ann x
  = let t   = annResult ann
    in  con1 (ann { annResult = canonT $ SumT ErrorT t } ) ConLeft x

substIntoIfDefinitely
        :: (Hashable n, Eq n)
        => Name n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
        -> Fresh n (Exp (Annot a n) n)
substIntoIfDefinitely var payload into
 | PossibilityDefinitely <- getPossibilityOrDefinitely $ annResult $ annotOfExp into
 = substInto var payload into
 | otherwise
 = return into


substInto
        :: (Hashable n, Eq n)
        => Name n
        -> Exp (Annot a n) n
        -> Exp (Annot a n) n
        -> Fresh n (Exp (Annot a n) n)
substInto var payload into
   -- This unsafe subst transform is safe as long as the payload only mentions fresh variable names
 = substX (Map.singleton var payload)
   into

