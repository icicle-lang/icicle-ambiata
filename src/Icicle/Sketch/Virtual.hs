-- | A rough sketch of expression language for virtual features.
-- This is not intended for human consumption.
-- However, it has the advantage of giving us a type checker for "free".
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Icicle.Sketch.Virtual where
-- | The top level for a virtual feature.
-- Parameterised by concrete feature's type and output type.
type TopLevel x u = Program x () u


-- | Program
--      (concrete feature type)
--      (scalar environment type)
--      (output type)
data Program :: * -> * -> * -> * where
 -- | Called "In" because it's the "In" of a "Let"
 In         :: ScalarExp t u
            -> Program x t u 
 -- | Scalar let
 LetS       :: ScalarExp t v
            -> Program x (v,t) u
            -> Program x t u
 -- | Reduce let.
 -- The reduce here cannot mention any previously bound scalars.
 LetR       :: Latest x v
            -> Program x (v,t) u
            -> Program x t u


data Latest :: * -> * -> * where
 Latest :: Int -> Transforms x t -> Reduce t u -> Latest x u

-- | All stream computations end with a fold
data Reduce :: * -> * -> * where
 Reduce :: ScalarExp  ()     a
        -> ScalarExp  (a,t)  a
        -> Reduce      t     a


-- | A chain of stream transformers with no fusion-preventing deps
data Transforms :: * -> * -> * where
 End    :: Transforms  t  t
 Chain  :: Transform   t  u
        -> Transforms  u  v
        -> Transforms  t  v


-- | A single stream transformer
-- These can all be fused into the final fold, of course,
-- but folds are rather annoying to compose.
data Transform :: * -> * -> * where
 Map    :: ScalarExp   t  u
        -> Transform   t  u
 Filter :: ScalarExp   t  Bool
        -> Transform   t  t


-- For now, we'll cheat and use Haskell functions as scalar expressions (HOAS)
type ScalarExp t u
 = t -> u

{-
-- | Scalar expression with list of available bindings
data ScalarExp :: [*] -> * -> * where
 Var    :: Index     ixs t
        -> ScalarExp ixs t

 BinOp  :: BinOp         t
        -> ScalarExp ixs t
        -> ScalarExp ixs t
        -> ScalarExp ixs t


-- | Primitive binary operators
data BinOp :: * -> * where
 Plus   :: BinOp Int
 Append :: BinOp String

-}
