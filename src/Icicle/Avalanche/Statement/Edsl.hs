-- | Crazy monadic combinators for writing Avalanche programs

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.Statement.Edsl (
    ValTypeOf, unsafeValType
  , A, unsafeA
  , X, unsafeX
  , S
  , acc, read, write, slet, sif, whileEq
  , xnum, xbool
  , statementOfS
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Annot

import              P
import              Data.Hashable                  (Hashable)

-- ----------------------------------------
-- Type synonyms with phantom types
-- ----------------------------------------
newtype ValTypeOf t
 = ValTypeOf ValType

unsafeValType :: ValType -> ValTypeOf t
unsafeValType = ValTypeOf


newtype A n t
 = A (Name n)

unsafeA :: Name n -> A n t
unsafeA = A

newtype X n t
 = X (Exp () n Prim)

unsafeX :: Exp a n Prim -> X n t
unsafeX = X . reannotX (const ())


-- ----------------------------------------
-- Monads!
-- The actual magic is a fresh monad with a continuation of what to do with the *rest* of the statement.
-- ----------------------------------------
newtype S n v
 = S (Fresh n (v, Statement () n Prim -> Statement () n Prim))

instance Monad (S n) where
 return v
  = S $ return (v, id)
 S a >>= f
  = S
  $ do  (v,a')  <- a
        let S f' = f v
        (v',b') <- f'
        -- Make sure apply b' first, so later declarations are nested
        return (v', a' . b')

-- ----------------------------------------
-- Helper functions
-- ----------------------------------------
freshS :: Hashable n => (Name n -> r) -> (Name n -> Statement () n Prim -> Statement () n Prim) -> S n r
freshS ret stmt
 = S
 $ do n <- fresh
      return (ret n, stmt n)

catS :: r -> Statement () n Prim -> S n r
catS ret stmt
 = S $ return (ret, \s -> stmt <> s)

catS' :: Statement () n Prim -> Fresh n ((), Statement () n Prim -> Statement () n Prim)
catS' stmt
 = return ((), \s -> stmt <> s)


-- ----------------------------------------
-- Statement generation functions.
-- This is the actually useful bit.
-- ----------------------------------------
acc :: Hashable n => ValTypeOf t -> X n t -> S n (A n t)
acc (ValTypeOf vt) (X val)
 = freshS A (\n -> InitAccumulator (Accumulator n vt val))

read :: Hashable n => ValTypeOf t -> A n t -> S n (X n t)
read (ValTypeOf vt) (A n)
 = freshS (\n' -> X (XVar () n')) (\n' -> Read n' n vt)

write :: A n t -> X n t -> S n ()
write (A n) (X x)
 = catS () (Write n x)

slet :: Hashable n => X n t -> S n (X n t)
slet (X x)
 = freshS (\n' -> X (XVar () n')) (\n' -> Let n' x)

sif :: X n Bool -> S n () -> S n () -> S n ()
sif (X x) (S s1) (S s2)
 = S
 $ do ((), s1') <- s1
      ((), s2') <- s2
      catS' (If x (s1' mempty) (s2' mempty))

whileEq :: A n t -> X n t -> S n () -> S n ()
whileEq (A n) (X x) (S s1)
 = S
 $ do ((), s1') <- s1
      catS' (While WhileEq n x (s1' mempty))

-- ----------------------------------------
-- Expression functions: TODO
-- ----------------------------------------
xnum  :: Int  -> X n Int
xnum  i = unsafeX (XValue () IntT (VInt i))
xbool :: Bool -> X n Bool
xbool b = unsafeX (XValue () BoolT (VBool b))

-- ----------------------------------------
-- Convert the thing to another thing
-- ----------------------------------------
statementOfS :: a -> S n () -> Fresh n (Statement a n Prim)
statementOfS a_fresh (S s)
 = do (_,s') <- s
      return $ reannotS (const a_fresh)
             $ s' mempty


-- ----------------------------------------
-- A dumb example
-- ----------------------------------------
_example_loop :: Hashable n => S n ()
_example_loop
 = do n <- acc intT (xnum 0)
      b <- acc boolT (xbool True)
      whileEq b (xbool True) $ do 
        y <- read intT n
        write n y
 where
  intT  = unsafeValType IntT :: ValTypeOf Int
  boolT = unsafeValType BoolT :: ValTypeOf Bool

