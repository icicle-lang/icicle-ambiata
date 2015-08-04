{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Base (
    CoreBinds    (..)
  , ConvertError (..)
  , annotOfError
  , ConvertM
  , ConvertState (..)
  , convertInput
  , convertInputName
  , convertInputType
  , convertWithInput
  , convertWithInputName
  , convertError
  , convertFeatures
  , convertModifyFeatures
  , convertFreshenAdd
  , convertFreshenLookup
  , convertValType

  , pre, strm, red, post
  , programOfBinds
  , convertWindowUnits
  ) where

import qualified        Icicle.Core             as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type
import qualified        Icicle.Common.Exp       as X
import qualified        Icicle.Core.Exp.Combinators as CE

import                  Icicle.Source.Query
import                  Icicle.Source.Type
import                  Icicle.Source.ToCore.Context

import                  Icicle.Internal.Pretty

import                  P

import                  Control.Monad.Trans.State.Lazy
import                  Control.Monad.Trans.Class

import qualified        Data.Map as Map


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
 | ConvertErrorPrimNoArguments a Int Prim
 | ConvertErrorGroupByHasNonGroupResult a (UniverseType n)
 | ConvertErrorContextNotAllowedInGroupBy a (Query (a,UniverseType n) n)
 | ConvertErrorExpNoSuchVariable a n
 | ConvertErrorExpNestedQueryNotAllowedHere a (Query (a,UniverseType n) n)
 | ConvertErrorExpApplicationOfNonPrimitive a (Exp (a,UniverseType n) n)
 | ConvertErrorReduceAggregateBadArguments a (Exp (a,UniverseType n) n)
 | ConvertErrorCannotConvertBaseType a (BaseType n)
 deriving (Show, Eq, Ord)

annotOfError :: ConvertError a n -> Maybe a
annotOfError e
 = case e of
    ConvertErrorNoSuchFeature _
     -> Nothing
    ConvertErrorPrimNoArguments a _ _
     -> Just a
    ConvertErrorGroupByHasNonGroupResult a _
     -> Just a
    ConvertErrorContextNotAllowedInGroupBy a _
     -> Just a
    ConvertErrorExpNoSuchVariable a _
     -> Just a
    ConvertErrorExpNestedQueryNotAllowedHere a _
     -> Just a
    ConvertErrorExpApplicationOfNonPrimitive a _
     -> Just a
    ConvertErrorReduceAggregateBadArguments a _
     -> Just a
    ConvertErrorCannotConvertBaseType a _
     -> Just a


type ConvertM a n r
 = StateT (ConvertState n)
 (FreshT n (Either (ConvertError a n))) r

data ConvertState n
 = ConvertState
 { csInputName  :: Name n
 , csInputType  :: ValType
 , csFeatures   :: FeatureContext n
 , csFreshen    :: Map.Map n (Name n)
 }

convertInput :: ConvertM a n (Name n, ValType)
convertInput
 = (,) <$> (csInputName <$> get) <*> (csInputType <$> get)

convertInputName :: ConvertM a n (Name n)
convertInputName
 = csInputName <$> get

convertInputType :: ConvertM a n ValType
convertInputType
 = csInputType <$> get

convertFeatures :: ConvertM a n (FeatureContext n)
convertFeatures
 = csFeatures <$> get


convertWithInputName :: Name n -> ConvertM a n r -> ConvertM a n r
convertWithInputName n c
 = do   t <- convertInputType
        convertWithInput n t c

convertWithInput :: Name n -> ValType -> ConvertM a n r -> ConvertM a n r
convertWithInput n t c
 = do   o <- get
        put (o { csInputName = n, csInputType = t })
        r <- c
        put o
        return r

convertModifyFeatures
        :: (FeatureContext n -> FeatureContext n)
        -> ConvertM a n ()
convertModifyFeatures f
 = do   o <- get
        put (o { csFeatures = f $ csFeatures o })

convertFreshenAdd :: Ord n => n -> ConvertM a n (Name n)
convertFreshenAdd prefix
 = do   n <- lift $ freshPrefix prefix
        o <- get
        put $ o { csFreshen = Map.insert prefix n $ csFreshen o }
        return n

convertFreshenLookup :: Ord n => a -> n -> ConvertM a n (Name n)
convertFreshenLookup ann n
 = do   o <- get
        case Map.lookup n $ csFreshen o of
         Nothing
          -> convertError $ ConvertErrorExpNoSuchVariable ann n
         Just n'
          -> return n'

convertValType :: a -> BaseType n -> ConvertM a n ValType
convertValType ann ty
 = case valTypeOfBaseType ty of
    Nothing
     -> convertError $ ConvertErrorCannotConvertBaseType ann ty
    Just t'
     -> return t'


convertError :: ConvertError a n -> ConvertM a n r
convertError = lift . lift . Left


convertWindowUnits :: WindowUnit -> C.Exp n
convertWindowUnits wu
 = CE.constI
 $ case wu of
    Days d -> d
    -- TODO: month should be... better
    Months m -> m * 30
    Weeks w -> w * 7




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

     ConvertErrorCannotConvertBaseType a t
      -> pretty a <> ": cannot convert BaseType: " <> pretty t

