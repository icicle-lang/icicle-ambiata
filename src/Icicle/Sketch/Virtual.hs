-- | A rough sketch of expression language for virtual features.
-- This is not intended for human consumption.
-- However, it has the advantage of giving us a type checker for "free".
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Icicle.Sketch.Virtual where


-- | For simplicity we'll only have a few types for now
data BaseType
 = IntT | StringT | BoolT
 | UnitT
 | PairT BaseType BaseType
 | ListT BaseType


-- | Convert our subset of types into Haskell types.
-- Really just for evaluation.
type family ValueOf (t :: BaseType) :: * where
 ValueOf IntT        = Int
 ValueOf StringT     = String
 ValueOf BoolT       = Bool
 ValueOf UnitT       = ()
 ValueOf (ListT t)   = [ValueOf t]
 ValueOf (PairT a b) = (ValueOf a, ValueOf b)


-- | Stratified so we can't have streams of streams
data Type
 = Scalar BaseType
 | Stream BaseType


-- | Either take the latest N entries, or the most recent.
data Virtual :: Type -> BaseType -> * where
 TakeN      :: Int
            -> Transforms t s
            -> Reduce s u
            -> Virtual (Stream t) u

{-
 - -- This is a boring case for now
 TakeLast   :: ScalarExp t        u
            -> ScalarExp UnitT    u
            -> Virtual (Scalar t) u
-}
  

-- | All stream computations end with a fold
data Reduce :: BaseType -> BaseType -> * where
 Reduce :: ScalarExp   UnitT a
        -> ScalarExp  (PairT a t) a
        -> Reduce      t     a


-- | A chain of stream transformers with no fusion-preventing deps
data Transforms :: BaseType -> BaseType -> * where
 End    :: Transforms  t  t
 Chain  :: Transform   t  u
        -> Transforms  u  v
        -> Transforms  t  v


-- | A single stream transformer
-- These can all be fused into the final fold, of course,
-- but folds are rather annoying to compose.
data Transform :: BaseType -> BaseType -> * where
 Map    :: ScalarExp   t  u
        -> Transform   t  u
 Filter :: ScalarExp   t  BoolT
        -> Transform   t  t


-- For now, we'll cheat and use Haskell functions as scalar expressions (HOAS)
type ScalarExp t u
 = ValueOf t -> ValueOf u

{-
-- | Scalar expression with list of available bindings
data ScalarExp :: [BaseType] -> BaseType -> * where
 Var    :: Index     ixs t
        -> ScalarExp ixs t

 BinOp  :: BinOp         t
        -> ScalarExp ixs t
        -> ScalarExp ixs t
        -> ScalarExp ixs t


-- | Primitive binary operators
data BinOp :: BaseType -> * where
 Plus   :: BinOp IntT
 Append :: BinOp StringT

-}
