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
  , convertFreshenLookupMaybe
  , convertValType

  , pre, strm, red, post
  , programOfBinds
  , convertWindowUnits
  ) where

import qualified        Icicle.Core             as C
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base
import                  Icicle.Common.Type  hiding (Type)
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


data CoreBinds a n
 = CoreBinds
 { precomps     :: [(Name n, C.Exp    a n)]
 , streams      :: [(Name n, C.Stream a n)]
 , reduces      :: [(Name n, C.Reduce a n)]
 , postcomps    :: [(Name n, C.Exp    a n)]
 }

programOfBinds
    :: OutputName
    -> ValType
    -> CoreBinds a n
    -> Maybe (Name n)
    -> a
    -> Name n
    -> C.Program a n
programOfBinds outputName inpType binds postdate a_ret ret
 = C.Program
 { C.input      = inpType
 , C.precomps   = precomps  binds
 , C.streams    = streams   binds
 , C.reduces    = reduces   binds
 , C.postcomps  = postcomps binds
 , C.postdate   = postdate
 , C.returns    = [(outputName, X.XVar a_ret ret)]
 }

instance Monoid (CoreBinds a n) where
 mempty = CoreBinds [] [] [] []
 mappend (CoreBinds a b c d) (CoreBinds f g h i)
  = CoreBinds (a<>f) (b<>g) (c<>h) (d<>i)


pre :: Name n -> C.Exp a n -> CoreBinds a n
pre n x = mempty { precomps = [(n,x)] }

strm :: Name n -> C.Stream a n -> CoreBinds a n
strm n x = mempty { streams = [(n,x)] }

red :: Name n -> C.Reduce a n -> CoreBinds a n
red n x = mempty { reduces = [(n,x)] }

post :: Name n -> C.Exp a n -> CoreBinds a n
post n x = mempty { postcomps = [(n,x)] }

data ConvertError a n
 = ConvertErrorNoSuchFeature (Name n)
 | ConvertErrorPrimNoArguments a Int Prim
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
 { csInputName  :: Name n
 , csInputType  :: ValType
 , csFeatures   :: FeatureContext () n
 , csFreshen    :: Map.Map (Name n) (Name n)
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

convertModifyFeatures
        :: (FeatureContext () n -> FeatureContext () n)
        -> ConvertM a n ()
convertModifyFeatures f
 = do   o <- get
        put (o { csFeatures = f $ csFeatures o })

convertFreshenAdd :: Ord n => Name n -> ConvertM a n (Name n)
convertFreshenAdd prefix
 = do   n <- lift $ freshPrefix' prefix
        o <- get
        put $ o { csFreshen  = Map.insert prefix n $ csFreshen  o
                , csFeatures = Map.delete prefix   $ csFeatures o }
        return n

convertFreshenLookup :: Ord n => a -> Name n -> ConvertM a n (Name n)
convertFreshenLookup ann n
 = do   o <- get
        case Map.lookup n $ csFreshen o of
         Nothing
          -> convertError $ ConvertErrorExpNoSuchVariable ann n
         Just n'
          -> return n'

convertFreshenLookupMaybe :: Ord n => Name n -> ConvertM a n (Maybe (Name n))
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


convertWindowUnits :: WindowUnit -> C.Exp () n
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

     ConvertErrorGroupFoldNotOnGroup a x
      -> pretty a <> ": group fold is not on a group; expected group but got " <> pretty x

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

     ConvertErrorCannotConvertType a t
      -> pretty a <> ": cannot convert base type: " <> pretty t

     ConvertErrorBadCaseNoDefault a x
      -> pretty a <> ": case has no default alternative: " <> pretty x
     ConvertErrorBadCaseNestedConstructors a x
      -> pretty a <> ": case has nested constructors in pattern; these should be removed by an earlier pass: " <> pretty x
     ConvertErrorImpossibleFold1 a
      -> pretty a <> ": fold1 cannot be converted; desugar first"


