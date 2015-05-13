-- | A-normalise expressions
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Simp.ANormal (
      anormal
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds
import Icicle.Common.Fresh

import P
import Data.List (unzip)

anormal :: Exp n p -> Fresh n (Exp n p)
anormal xx
 = do   (bs, x) <- anormal' xx
        return $ makeLets bs x


anormal' :: Exp n p -> Fresh n ([(Name n, Exp n p)], Exp n p)
anormal' xx
 = case xx of
    XVar{}
     -> ret
    XPrim{}
     -> ret
    XValue{}
     -> ret

    XApp{}
     -> do  let (f,args) = takeApps xx
            (bf, xf)    <-      extractBinding f
            (bs, xs)    <- unzip <$> mapM extractBinding args
            return (concat (bf:bs), makeApps xf xs)

    XLam n v x
     -> do  x' <- anormal x
            return ([], XLam n v x')

    XLet n x y
     -> do  (bx, x')    <- anormal' x
            (by, y')    <- anormal' y
            -- TODO: I think we need to rename in here
            return (bx <> [(n, x')] <> by, y')

 where
  ret = return ([], xx)



extractBinding :: Exp n p -> Fresh n ([(Name n, Exp n p)], Exp n p)
extractBinding xx
 | isNormal xx
 = return ([], xx)
 | otherwise
 = do   (bs,x') <- anormal' xx
        n       <- fresh
        return (bs <> [(n,x')], XVar n)

isNormal :: Exp n p -> Bool
isNormal xx
 = case xx of
    XVar{}   -> True
    XPrim{}  -> True
    XValue{} -> True
    XApp{}   -> False
    XLam{}   -> False
    XLet{}   -> False
