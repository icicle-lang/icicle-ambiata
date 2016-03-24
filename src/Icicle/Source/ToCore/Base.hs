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
  , convertDateName
  , convertFactIdName
  , convertFactTimeName
  , convertWithInput
  , convertWithInputName
  , convertError
  , convertFeatures
  , convertModifyFeatures
  , convertModifyFeaturesMap
  , convertFreshenAdd
  , convertFreshenAddAs
  , convertFreshenLookup
  , convertFreshenLookupMaybe
  , convertValType
  , convertContext

  , pre, filt, sfold, post
  , programOfBinds
  , pullPosts
  ) where

import qualified        Icicle.Core             as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type  hiding (Type)
import qualified        Icicle.Common.Exp       as X

import                  Icicle.Source.Query
import                  Icicle.Source.Type
import                  Icicle.Source.ToCore.Context

import                  Icicle.Internal.Pretty

import                  P

import                  Control.Monad.Trans.State.Lazy
import                  Control.Monad.Trans.Class

import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


data CoreBinds a n
 = CoreBinds
 { precomps     :: [(Name n, C.Exp    a n)]
 , streams      :: [C.Stream a n]
 , postcomps    :: [(Name n, C.Exp    a n)]
 }

programOfBinds
    :: OutputName
    -> ValType
    -> Name n
    -> Name n
    -> Name n
    -> Name n
    -> CoreBinds a n
    -> a
    -> Name n
    -> C.Program a n
programOfBinds outputName inpType factValName factIdName factTimeName postDate binds a_ret ret
 = C.Program
 { C.inputType    = inpType
 , C.factValName  = factValName
 , C.factIdName   = factIdName
 , C.factTimeName = factTimeName
 , C.snaptimeName = postDate
 , C.precomps     = precomps  binds
 , C.streams      = streams   binds
 , C.postcomps    = postcomps binds
 , C.returns      = [(outputName, X.XVar a_ret ret)]
 }


-- | Rip out the postcomputations into lets.
-- The result expression has the postcomputations as lets,
-- the result bindings have no postcomputations.
pullPosts :: a
          -> (CoreBinds a n, Name n)
          -> (CoreBinds a n, C.Exp a n)
pullPosts a (bs,ret)
 = let ps  = postcomps bs
       bs' = bs { postcomps = [] }
   in  (bs', X.makeLets a ps $ X.XVar a ret)


instance Monoid (CoreBinds a n) where
 mempty = CoreBinds [] [] []
 mappend (CoreBinds a b c) (CoreBinds d e f)
  = CoreBinds (a<>d) (b<>e) (c<>f)


pre :: Name n -> C.Exp a n -> CoreBinds a n
pre n x = mempty { precomps = [(n,x)] }

filt :: C.Exp a n -> [C.Stream a n] -> CoreBinds a n
filt p s
 = mempty { streams = [C.SFilter p s] }

sfold :: Name n -> ValType -> C.Exp a n -> C.Exp a n -> CoreBinds a n
sfold n t z k = mempty { streams = [C.SFold n t z k] }

post :: Name n -> C.Exp a n -> CoreBinds a n
post n x = mempty { postcomps = [(n,x)] }

data ConvertError a n
 = ConvertErrorNoSuchFeature (Name n)
 | ConvertErrorPrimNoArguments a Int Prim
 | ConvertErrorPrimAggregate   a Prim
 | ConvertErrorGroupByHasNonGroupResult a (Type n)
 | ConvertErrorGroupFoldNotOnGroup a (Exp (Annot a n) n)
 | ConvertErrorContextNotAllowedInGroupBy a (Query (Annot a n) n)
 | ConvertErrorExpNoSuchVariable a (Name n)
 | ConvertErrorExpNestedQueryNotAllowedHere a (Query (Annot a n) n)
 | ConvertErrorExpApplicationOfNonPrimitive a (Exp (Annot a n) n)
 | ConvertErrorReduceAggregateBadArguments a (Exp (Annot a n) n)
 | ConvertErrorCannotConvertType a (Type n)
 | ConvertErrorBadCaseNoDefault a (Exp (Annot a n) n)
 | ConvertErrorBadCaseNestedConstructors a (Exp (Annot a n) n)
 | ConvertErrorImpossibleFold1 a
 deriving (Show, Eq, Ord)

annotOfError :: ConvertError a n -> Maybe a
annotOfError e
 = case e of
    ConvertErrorNoSuchFeature _
     -> Nothing
    ConvertErrorPrimNoArguments a _ _
     -> Just a
    ConvertErrorPrimAggregate a _
     -> Just a
    ConvertErrorGroupByHasNonGroupResult a _
     -> Just a
    ConvertErrorGroupFoldNotOnGroup a _
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
    ConvertErrorCannotConvertType a _
     -> Just a
    ConvertErrorBadCaseNoDefault a _
     -> Just a
    ConvertErrorBadCaseNestedConstructors a _
     -> Just a
    ConvertErrorImpossibleFold1 a
     -> Just a


type ConvertM a n r
 = StateT (ConvertState n)
 (FreshT n (Either (ConvertError a n))) r

data ConvertState n
 = ConvertState
 { csInputName    :: Name n
 , csInputType    :: ValType
 , csFactIdName   :: Name n
 , csFactTimeName :: Name n
 , csDateName     :: Name n
 , csFeatures     :: FeatureContext () n
 , csFreshen      :: Map.Map (Name n) (Name n)
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

convertFactIdName :: ConvertM a n (Name n)
convertFactIdName
 = csFactIdName <$> get

convertFactTimeName :: ConvertM a n (Name n)
convertFactTimeName
 = csFactTimeName <$> get


convertDateName :: ConvertM a n (Name n)
convertDateName
 = csDateName <$> get

convertFeatures :: ConvertM a n (FeatureContext () n)
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

convertContext
        :: ConvertM a n r
        -> ConvertM a n r
convertContext with
 = do   o <- get
        r <- with
        put o
        return r

convertModifyFeatures
        :: (FeatureContext () n -> FeatureContext () n)
        -> ConvertM a n ()
convertModifyFeatures f
 = do   o <- get
        put (o { csFeatures = f $ csFeatures o })

-- | When adding a variable binding, you need to modify the existing things, and then add something new.
-- The variable you're adding also needs to be cleared from any other environments, for shadowing to work
convertModifyFeaturesMap
        :: (Hashable n, Eq n)
        => (Map.Map (Name n) (FeatureVariable () n) -> Map.Map (Name n) (FeatureVariable () n))
        -> Name n
        -> ConvertM a n ()
convertModifyFeaturesMap f clearName
 = do   o <- get
        let fs = csFeatures o
        let fv = featureContextVariables fs
        put (o { csFeatures = fs { featureContextVariables = f fv }
               -- Remove this from the freshen environment as the features map version shadows it
               , csFreshen  = Map.delete clearName $ csFreshen o })


convertFreshenAdd :: (Hashable n, Eq n) => Name n -> ConvertM a n (Name n)
convertFreshenAdd prefix
 = do   n <- lift $ freshPrefixBase $ nameBase prefix
        convertFreshenAddAs prefix n
        return n

convertFreshenAddAs :: (Hashable n, Eq n) => Name n -> Name n -> ConvertM a n ()
convertFreshenAddAs from to
 = do   o <- get
        put $ o { csFreshen  = Map.insert from to $ csFreshen  o }


convertFreshenLookup :: (Hashable n, Eq n) => a -> Name n -> ConvertM a n (Name n)
convertFreshenLookup ann n
 = do   o <- get
        case Map.lookup n $ csFreshen o of
         Nothing
          -> convertError $ ConvertErrorExpNoSuchVariable ann n
         Just n'
          -> return n'

convertFreshenLookupMaybe :: (Hashable n, Eq n) => Name n -> ConvertM a n (Maybe (Name n))
convertFreshenLookupMaybe n
 = do   o <- get
        return $ Map.lookup n $ csFreshen o


convertValType :: a -> Type n -> ConvertM a n ValType
convertValType ann ty
 = case valTypeOfType ty of
    Nothing
     -> convertError $ ConvertErrorCannotConvertType ann ty
    Just t'
     -> return t'


convertError :: ConvertError a n -> ConvertM a n r
convertError = lift . lift . Left

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

     ConvertErrorPrimAggregate a p
      -> pretty a <> ": primitive " <> pretty p <> " is an aggregate. It should have been handled earlier."
     ConvertErrorGroupByHasNonGroupResult a ut
      -> pretty a <> ": group by has wrong return type; should be a group but got " <> pretty ut

     ConvertErrorGroupFoldNotOnGroup a x
      -> pretty a <> ": group fold is not on a group; expected group but got " <> pretty x

     ConvertErrorContextNotAllowedInGroupBy a q -> pretty a <> ": only filters and aggregates are allowed in group by (the rest are TODO): " <> pretty q

     ConvertErrorExpNoSuchVariable a n
      -> pretty a <> ": no such variable " <> pretty n

     ConvertErrorExpNestedQueryNotAllowedHere a q
      -> pretty a <> ": nested query not allowed in this expression: " <> pretty q

     ConvertErrorExpApplicationOfNonPrimitive a x
      -> pretty a <> ": application of non-function: " <> pretty x

     ConvertErrorReduceAggregateBadArguments a x
      -> pretty a <> ": bad arguments to aggregate: " <> pretty x

     ConvertErrorCannotConvertType a t
      -> pretty a <> ": cannot convert base type: " <> pretty t

     ConvertErrorBadCaseNoDefault a x
      -> pretty a <> ": case has no default alternative: " <> pretty x
     ConvertErrorBadCaseNestedConstructors a x
      -> pretty a <> ": case has nested constructors in pattern; these should be removed by an earlier pass: " <> pretty x
     ConvertErrorImpossibleFold1 a
      -> pretty a <> ": fold1 cannot be converted; desugar first"


