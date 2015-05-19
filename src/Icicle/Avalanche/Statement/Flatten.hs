-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten (
    flatten
  ) where

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat

import qualified    Icicle.Core.Exp.Prim           as Core

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              Icicle.Internal.Pretty

import              P
import              Control.Monad.Trans.Class
import              Data.List (reverse)
import              Data.String (String)

data FlattenError n
 = FlattenError String
 | FlattenErrorPrimBadArgs Core.Prim [Exp n Core.Prim]
 deriving (Eq, Ord, Show)

type FlatM n
 = FreshT n (Either (FlattenError n)) (Statement n Flat.Prim)

flatten :: (Ord n, Pretty n)
        => Statement n Core.Prim
        -> FlatM n
flatten s
 = case s of
    If x ts es
     -> flatX x (\x' -> If x' <$> flatten ts <*> flatten es)

    Let n x ss
     -> flatX x (\x' -> Let n x' <$> flatten ss)

    ForeachInts n from to ss
     -> flatX from
     $ \from'
     -> flatX to
     $ \to'
     -> ForeachInts n from' to' <$> flatten ss

    ForeachFacts n vt ss
     ->     ForeachFacts n vt <$> flatten ss

    Block ss
     ->     Block <$> mapM flatten ss

    InitAccumulator acc ss
     -> flatX (accInit acc)
     $ \x'
     -> InitAccumulator (acc { accInit = x' }) <$> flatten ss

    Read n m ss
     ->     Read n m <$> flatten ss

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


flatX   :: (Ord n, Pretty n)
        => Exp n Core.Prim
        -> (Exp n Flat.Prim -> FlatM n)
        -> FlatM n

flatX xx stm
 = convX
 where
  x' = beta isSimpleValue
     $ betaToLets xx

  convX
   = case x' of
      XVar n
       -> stm $ XVar n
      XValue vt bv
       -> stm $ XValue vt bv

      XApp{}
       | Just (p,xs) <- takePrimApps x'
       -> flatPrim p xs

       | otherwise
       -> lift $ Left $ FlattenError ("Flatten: TODO: application of non-primitive: " <> show (pretty $ xx, pretty x'))

      XPrim p
       -> flatPrim p []

      XLam{}
       -> lift $ Left $ FlattenError ("Flatten: TODO: application of non-primitive: " <> show (pretty $ xx, pretty x'))


      XLet n p q
       -> flatX p
       $ \p'
       -> Let n p' <$> flatX q stm

  flatPrim p xs
   = case p of
      Core.PrimMinimal pm
       -> primApps (Flat.PrimMinimal pm) xs []

      Core.PrimFold pf ta
       -> flatFold pf ta xs
      Core.PrimMap (Core.PrimMapInsertOrUpdate tk tv)
       | [upd, ins, key, map]   <- xs
       -> flatX key
       $ \key'
       -> flatX map
       $ \map'
       -> do    nexist <- fresh
                let xexist = XPrim (Flat.PrimProject (Flat.PrimProjectMapLookup tk tv))
                                 `makeApps` [map', key']

                let xisSome  = XPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome tv))
                         `makeApps` [XVar nexist]

                -- Only get the value inside the some branch
                nexistSome <- fresh
                let xexistSome = XPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet tv))
                                `makeApps` [XVar nexist]

                -- Name for the actual value we're storing
                nputval <- fresh

                nput <- fresh
                -- Perform put; nputval needs to be bound with actual value before
                let xput = XPrim (Flat.PrimUpdate (Flat.PrimUpdateMapPut tk tv))
                         `makeApps` [map', key', XVar nputval]
                

                upd' <- flatX (upd `makeApps` [XVar nexistSome]) $ \upd'
                 ->      Let nputval upd'
                      .  Let nput    xput
                     <$> flatX (XVar nput) stm

                ins'  <- flatX ins $ \ins'
                 ->      Let nputval ins'
                      .  Let nput    xput
                     <$> flatX (XVar nput) stm

                return
                 $ Let nexist xexist
                 $ If xisSome
                     ( Let nexistSome xexistSome upd')
                     ( ins' )

      _
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

  primApps p [] conv
   = stm
   $ makeApps (XPrim p)
   $ reverse conv

  primApps p (a:as) conv
   = flatX a
   $ \a'
   -> primApps p as (a' : conv)

  flatFold Core.PrimFoldBool _ [the, els, bo]
   -- XXX: we are using "stm" twice here,
   -- so duplicating branches.
   -- I don't think this is a biggie
   -- (yet)
   = flatX bo
   $ \bo'
   -> do    the'  <- flatX the stm
            els'  <- flatX els stm
            return $ If bo' the' els'

  flatFold (Core.PrimFoldPair ta tb) _ [fun, pr]
   =     flatX pr
        $ \pr'
        -> do   p1 <- fresh
                p2 <- fresh
                let fun' = makeApps fun [XVar p1, XVar p2]
                s' <- flatX (fun') stm
                
                return
                  $ Let p1 (proj False ta tb pr')
                  $ Let p2 (proj True  ta tb pr')
                      s'

  flatFold (Core.PrimFoldArray telem) tacc [k, z, arr]
   = do accN <- fresh
        iter <- fresh
        elm  <- fresh

        stm' <- stm (XVar accN)


        -- Crumbs!
        loop <- flatX arr $ \arr'
         -> let len = XPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength telem) `XApp` arr'
            in  ForeachInts iter (XValue IntT (VInt 0)) len
             .  Read accN accN
             .  Let  elm  (makeApps
                                (XPrim (Flat.PrimUnsafe $ Flat.PrimUnsafeArrayIndex telem))
                                [arr', XVar iter])
            <$> flatX (makeApps k [ XVar accN, XVar elm  ])
                      (return . Write accN)

        flatX z $ \z' ->
            return (InitAccumulator (Accumulator accN Mutable tacc z')
                   (loop <> Read accN accN stm'))


  flatFold (Core.PrimFoldMap tk tv) tacc [k, z, arr]
   = do accN <- fresh
        iter <- fresh
        elm  <- fresh
        efst <- fresh
        esnd <- fresh

        stm' <- stm (XVar accN)


        -- Crumbs! Copy and paste not so good!
        loop <- flatX arr $ \arr'
         -> let len = XPrim (Flat.PrimProject $ Flat.PrimProjectMapLength tk tv) `XApp` arr'
            in  ForeachInts iter (XValue IntT (VInt 0)) len
             .  Read accN accN
             .  Let  elm  (makeApps
                                (XPrim (Flat.PrimUnsafe $ Flat.PrimUnsafeMapIndex tk tv))
                                [arr', XVar iter])
             .  Let  efst (proj False tk tv $ XVar elm )
             .  Let  esnd (proj True  tk tv $ XVar elm )
            <$> flatX (makeApps k
                                [ XVar accN
                                , XVar efst
                                , XVar esnd ])
                      (return . Write accN)

        flatX z $ \z' ->
            return (InitAccumulator (Accumulator accN Mutable tacc z')
                   (loop <> Read accN accN stm'))



  flatFold pf _ _
   = primApps (Flat.PrimTODO (show pf)) [] []

  proj t ta tb e
   = (XPrim
    $ Flat.PrimProject
    $ Flat.PrimProjectPair t ta tb)
    `XApp` e

