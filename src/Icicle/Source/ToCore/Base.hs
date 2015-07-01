{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Base (
    CoreBinds    (..)
  , ConvertError (..)
  , ConvertM
  , pre, strm, red, post
  , programOfBinds
  , freshly
  , convertWindowUnits
  , baseTypeOrOption
  , applyPossibles
  ) where

import qualified        Icicle.Core             as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type
import qualified        Icicle.Common.Exp       as X
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Type      as T
import qualified        Icicle.Core.Exp.Combinators as CE

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Internal.Pretty

import                  P


data CoreBinds n
 = CoreBinds
 { precomps     :: [(Name n, C.Exp n)]
 , streams      :: [(Name n, C.Stream n)]
 , reduces      :: [(Name n, C.Reduce n)]
 , postcomps    :: [(Name n, C.Exp n)]
 }

programOfBinds
    :: ValType
    -> CoreBinds n
    -> Name n
    -> C.Program n
programOfBinds inpType binds ret
 = C.Program
 { C.input      = inpType
 , C.precomps   = precomps  binds
 , C.streams    = streams   binds
 , C.reduces    = reduces   binds
 , C.postcomps  = postcomps binds
 , C.postdate   = Nothing
 , C.returns    = X.XVar ret
 }

instance Monoid (CoreBinds n) where
 mempty = CoreBinds [] [] [] []
 mappend (CoreBinds a b c d) (CoreBinds e f g h)
  = CoreBinds (a<>e) (b<>f) (c<>g) (d<>h)


pre :: Name n -> C.Exp n -> CoreBinds n
pre n x = mempty { precomps = [(n,x)] }

strm :: Name n -> C.Stream n -> CoreBinds n
strm n x = mempty { streams = [(n,x)] }

red :: Name n -> C.Reduce n -> CoreBinds n
red n x = mempty { reduces = [(n,x)] }

post :: Name n -> C.Exp n -> CoreBinds n
post n x = mempty { postcomps = [(n,x)] }



data ConvertError a n
 = ConvertErrorNoSuchFeature n
 | ConvertErrorPrimAggregateNotAllowedHere a Agg
 | ConvertErrorPrimNoArguments a Int Prim
 | ConvertErrorGroupByHasNonGroupResult a UniverseType
 | ConvertErrorContextNotAllowedInGroupBy a (Query (a,UniverseType) n)
 | ConvertErrorExpNoSuchVariable a n
 | ConvertErrorExpNestedQueryNotAllowedHere a (Query (a,UniverseType) n)
 | ConvertErrorExpApplicationOfNonPrimitive a (Exp (a,UniverseType) n)
 | ConvertErrorReduceAggregateBadArguments a (Exp (a,UniverseType) n)
 deriving (Show, Eq, Ord)


type ConvertM a n r
 = FreshT n (Either (ConvertError a n)) r

freshly :: (Name n -> r) -> ConvertM a n (r, Name n)
freshly f
 = do   n' <- fresh
        return (f n', n')


convertWindowUnits :: WindowUnit -> C.Exp n
convertWindowUnits wu
 = CE.constI
 $ case wu of
    Days d -> d
    -- TODO: month should be... better
    Months m -> m * 30
    Weeks w -> w * 7

baseTypeOrOption :: UniverseType -> BaseType
baseTypeOrOption u
 | Possibly <- universePossibility $ universe u
 = T.OptionT $ baseType u
 | otherwise
 = baseType u


-- | Apply a function to some arguments, depending on the arguments' "possibility".
-- If any of the arguments are "possibly", unwrap their possibilities and
-- rewrap the entire expression in a possibility.
--
-- The function type here is the "raw" return type of the function.
-- That is, its modality/possibility is unaffected by those of
-- its arguments.
--
-- This means for division, the modality should be "Possible",
-- but for addition the modality should be "Definitely"
-- even if the addition is applied to Possible arguments,
-- and the actual application's return type would be Possible.
--
applyPossibles  :: C.Exp n
                -> UniverseType
                -> [(C.Exp n, UniverseType)]
                -> ConvertM a n (C.Exp n)
applyPossibles f returns xts
 -- Go through xts, building up the list of actual expressions
 -- to apply as arguments, and keep track of whether we've
 -- seen any possibles so far.
 = go xts [] False

 where
  -- At the end of xts, apply the function to the arguments
  go [] args anyPossibles
   -- If we've seen any possibles, and the raw function itself
   -- returns a definitely, we need to wrap the result
   -- in a "Some" constructor.
   | anyPossibles
   , Definitely <- universePossibility $ universe returns
   = return $ mkSome $ X.makeApps f args
   -- Otherwise, we have seen no possibles or the raw function
   -- itself returns possibles, so we can leave the return as-is.
   | otherwise
   = return $          X.makeApps f args

  -- For each entry of xts..
  go ((x,u):xts') args anyPossibles
   -- This argument is a possible, so we need to unwrap it.
   | Possibly <- universePossibility $ universe u
   = do -- Generate a fresh name, and apply the function using that name.
        -- Note that we have seen a possible and may need a "Some" wrapper.
        n' <- fresh
        f' <- go xts' (args <> [X.XVar n']) True

        -- Use "FoldOption", equivalent to "maybe", to unwrap.
        let nt   = baseType u
        let retty= T.OptionT $ baseType returns
        let opt  = C.PrimFold (C.PrimFoldOption nt) retty

        -- Bind the fresh name in the function to fold
        let fun  = X.XLam n' nt f'
        let none = X.XValue retty VNone

        -- Perform the option fold on the original expression
        let wrap = X.makeApps (X.XPrim opt)
                 [ fun, none, x ]

        return wrap
   
   -- Simple case, it isn't a possible so we just apply the function
   | otherwise
   = go xts' (args <> [x]) anyPossibles

  -- Primitive for Some (Just) constructor.
  mkSome x
   = X.XPrim (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstSome $ baseType returns)
    `X.XApp`  x
        

-- | These errors should only occur if
--   - there is a bug in the conversion (there is)
--   - or the program shouldn't type check
--
--   so the pretty printing doesn't have to be as good as type checking.
instance (Pretty a, Pretty n) => Pretty (ConvertError a n) where
 pretty e
  = case e of
     ConvertErrorNoSuchFeature n
      -> "No such feature: " <> pretty n

     ConvertErrorPrimAggregateNotAllowedHere a agg
      -> pretty a <> ": aggregate " <> pretty agg <> " not allowed in expression"
    
     ConvertErrorPrimNoArguments a num_args p
      -> pretty a <> ": primitive " <> pretty p <> " expects " <> pretty num_args <> " arguments but got none"

     ConvertErrorGroupByHasNonGroupResult a ut
      -> pretty a <> ": group by has wrong return type; should be a group but got " <> pretty ut
     
     ConvertErrorContextNotAllowedInGroupBy a q
      -> pretty a <> ": only filters and aggregates are allowed in group by (the rest are TODO): " <> pretty q

     ConvertErrorExpNoSuchVariable a n
      -> pretty a <> ": no such variable " <> pretty n

     ConvertErrorExpNestedQueryNotAllowedHere a q
      -> pretty a <> ": nested query not allowed in this expression: " <> pretty q

     ConvertErrorExpApplicationOfNonPrimitive a x
      -> pretty a <> ": application of non-function: " <> pretty x

     ConvertErrorReduceAggregateBadArguments a x
      -> pretty a <> ": bad arguments to aggregate: " <> pretty x

