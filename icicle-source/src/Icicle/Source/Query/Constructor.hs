-- | Constructors, like Some, None, tuples etc
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Query.Constructor (
    Constructor (..)
  , Pattern (..)
  , Lit (..)
  , substOfPattern
  , boundOfPattern
  , arityOfConstructor
  ) where

import           Data.Text (unpack)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Data.Time
import           Icicle.Internal.Pretty
import           Icicle.Source.Query.Operators

import           P

data Lit
 = LitInt Int
 | LitDouble Double
 | LitString Text
 | LitTime Time
 deriving (Show, Eq, Ord, Generic)

instance NFData Lit

data Constructor
 -- Option
 = ConSome
 | ConNone

 -- ,
 | ConTuple

 -- Bool
 | ConTrue
 | ConFalse

 -- Either
 | ConLeft
 | ConRight

 -- Error
 | ConError ExceptionInfo
 deriving (Eq, Ord, Show, Generic)

instance NFData Constructor

data Pattern n
 = PatCon Constructor [Pattern n]
 | PatLit Lit Bool -- Negation marker
 | PatDefault
 | PatVariable (Name n)
 deriving (Show, Eq, Ord, Generic)

instance NFData n => NFData (Pattern n)

boundOfPattern :: Eq n => Pattern n -> Set.Set (Name n)
boundOfPattern p = case p of
 PatCon _ ps   -> Set.unions $ fmap boundOfPattern ps
 PatLit _ _    -> Set.empty
 PatDefault    -> Set.empty
 PatVariable n -> Set.singleton n

-- | Given a pattern and a value,
-- check if the value matches the pattern, and if so,
-- return a mapping from pattern names to the sub-values.
-- For example
-- > substOfPattern (True,a) (True, False)
-- > = Just { a => False }
substOfPattern :: Ord n => Pattern n -> BaseValue -> Maybe (Map.Map (Name n) BaseValue)
substOfPattern PatDefault _
 = return Map.empty
substOfPattern (PatVariable n) v
 = return (Map.singleton n v)

substOfPattern (PatLit (LitInt i) neg) val
 | VInt i'   <- val
 , (if neg then negate else id) i == i'
 = return Map.empty
substOfPattern (PatLit (LitDouble d) neg) val
 | VDouble d'   <- val
 , (if neg then negate else id) d == d'
 = return Map.empty
substOfPattern (PatLit (LitString s) _) val
 | VString s'   <- val
 , s == s'
 = return Map.empty
substOfPattern (PatLit (LitTime t) _) val
 | VTime t'   <- val
 , t == t'
 = return Map.empty
substOfPattern (PatLit _ _) _
 = Nothing

substOfPattern (PatCon ConSome pats) val
 | [pa]         <- pats
 , VSome va     <- val
 = substOfPattern pa va
 | otherwise
 = Nothing

substOfPattern (PatCon ConNone pats) val
 | []           <- pats
 , VNone        <- val
 = return Map.empty
 | otherwise
 = Nothing

substOfPattern (PatCon ConTuple pats) val
 | [pa, pb]     <- pats
 , VPair va vb  <- val
 = Map.union <$> substOfPattern pa va <*> substOfPattern pb vb
 | otherwise
 = Nothing

substOfPattern (PatCon ConTrue pats) val
 | []           <- pats
 , VBool True   <- val
 = return Map.empty
 | otherwise
 = Nothing

substOfPattern (PatCon ConFalse pats) val
 | []           <- pats
 , VBool False  <- val
 = return Map.empty
 | otherwise
 = Nothing

substOfPattern (PatCon ConLeft pats) val
 | [pa]         <- pats
 , VLeft va     <- val
 = substOfPattern pa va
 | otherwise
 = Nothing

substOfPattern (PatCon ConRight pats) val
 | [pa]         <- pats
 , VRight va    <- val
 = substOfPattern pa va
 | otherwise
 = Nothing

substOfPattern (PatCon (ConError ex) pats) val
 | []             <- pats
 , VError ex'     <- val
 , ex == ex'
 = return Map.empty
 | otherwise
 = Nothing


arityOfConstructor :: Constructor -> Int
arityOfConstructor cc
 = case cc of
    ConSome  -> 1
    ConNone  -> 0

    ConTuple -> 2

    ConTrue  -> 0
    ConFalse -> 0

    ConLeft  -> 1
    ConRight -> 1

    ConError _ -> 0


instance Pretty Lit where
  pretty = \case
    LitInt i ->
      annotate AnnConstant . text $ show i
    LitDouble i ->
      annotate AnnConstant . text $ show i
    LitString i ->
      annotate AnnConstant . text $ show i
    LitTime i ->
      annotate AnnConstant $
        "`" <> (text $ unpack $ renderTime i) <> "`"

instance Pretty Constructor where
  pretty = \case
    ConSome ->
      prettyConstructor "Some"
    ConNone ->
      prettyConstructor "None"

    ConTuple ->
      prettyConstructor "Tuple"

    ConTrue ->
      prettyConstructor "True"
    ConFalse ->
      prettyConstructor "False"

    ConLeft ->
      prettyConstructor "Left"
    ConRight ->
      prettyConstructor "Right"
    ConError e ->
      prettyConstructor $ show e

instance Pretty n => Pretty (Pattern n) where
  prettyPrec p = \case
    PatCon ConTuple [a,b] ->
      prettyPrec p (a, b)
    PatCon c [] ->
      prettyPrec p c
    PatCon c vs ->
      prettyApp hsep p c vs
    PatDefault ->
      prettyPunctuation "_"
    PatLit l neg ->
      if neg then
        prettyApp hsep p (ArithUnary Negate) [l]
      else
        prettyPrec p l
    PatVariable n ->
      annotate AnnBinding (pretty n)
