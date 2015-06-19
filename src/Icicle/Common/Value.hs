-- | Values that are common across all languages
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Value (
      Heap
    , Value     (..)
    , getBaseValue
    , typeOfValue
    ) where

import           Icicle.Common.Base
import           Icicle.Common.Exp.Exp
import           Icicle.Common.Type
import           Icicle.Internal.Pretty

import           P

import           Data.Either.Combinators (fromRight')
import qualified Data.List               as L
import qualified Data.Map                as Map


-- | A heap is just a mapping from names to values.
type Heap n p
 = Map.Map (Name n) (Value n p)


-- | A top-level value can be a function/closure or a base value
data Value n p
 = VBase  BaseValue
 -- | A function carries its own heap, the name of its argument, and the expression to apply.
 -- Actually - we might want the type of the argument here too, for typeOfValue
 | VFun   (Heap n p)  (Name n)  (Exp n p)
 deriving (Show, Ord, Eq)


getBaseValue :: e -> Value n p -> Either e BaseValue
getBaseValue e VFun{}    = Left e
getBaseValue _ (VBase v) = Right v

typeOfValue :: e -> Value n p -> Either e ValType
typeOfValue e (VBase v) = typeOfBaseValue e v
typeOfValue e VFun{}    = Left e

typeOfBaseValue :: e -> BaseValue -> Either e ValType
typeOfBaseValue e v = case v of
  VInt  _      -> return IntT
  VBool _      -> return BoolT
  VDateTime _  -> return DateTimeT

  VPair v1 v2  -> PairT   <$> typeOfBaseValue e v1 <*> typeOfBaseValue e v2

  VSome v1     -> OptionT <$> typeOfBaseValue e v1
  VNone        -> OptionT <$> pure IntT -- I don't know, do we have bottoms?

  VArray vs
   | ts <- L.map (typeOfBaseValue e) vs, t:_ <- ts, allSame ts
   -> ArrayT <$> t
   | otherwise
   -> Left e

  VMap _
    -> Left e -- I don't know, what if map is empty?

  where allSame xs
          | all isRight xs
          , rs <- L.map fromRight' xs
          = L.all ((==)1 . L.length) . L.group $ rs
          | otherwise = False

instance (Pretty n, Pretty p) => Pretty (Value n p) where
 pretty (VBase b) = pretty b
 pretty (VFun h n x) = text "closure#" <+> pretty (Map.toList h) <+> text "\\" <> pretty n <> text ". " <> pretty x
