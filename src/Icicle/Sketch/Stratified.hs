{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Icicle.Sketch.Stratified where

-- Stratification:
-- there are four or so stages of execution:
--  - pre-scalars, these are really constants but might be useful;
--  - streams and online stream transformers like map and filter;
--  - reductions of streams;
--  - post-scalars, where multiple reduction results could be merged
--
-- so I figure that something like the following:
--
-- SELECT sum(x) / count(x)
--      , x = y * 2
-- WHERE  y > 52 - 13
--
-- would be split up as
--
--        333 2  4 33333 2
-- SELECT sum(x) / count(x)
--        222222222
--      , x = y * 2
--        222 1111111
-- WHERE  y > 52 - 13
--

{-
 - -- Easier without datakinds for now
data Stage =
   Pre
 | Stream
 | Post
-}

-- Stages
data Pre t
data Stream t
data Post t

-- Get the value type of a pre or post scalar
type family GetScalar t where
 GetScalar (Pre t)        = t
 GetScalar (Post t)       = t

-- Replace a pre or post scalar with another type,
-- but stay as pre or post
type family ReplaceScalar t u where
 ReplaceScalar (Pre t)  u = Pre t
 ReplaceScalar (Post t) u = Post u

-- De bruijn indices.
-- Use a list of types as the environment,
-- then the index is the variable name
data Index :: [*] -> * -> * where
 Here  :: Index (x ': xs) x
 There :: Index xs x -> Index (y ': xs) x


-- The top-level expression has to be a single post-scalar
-- based on concrete feature "x".
type TopLevel x u = Exp x '[] (Post u)

-- Exp
--      (concrete feature element type)
--      (environment list of staged types)
--      (output type and stage)
--
data Exp :: * -> [*] -> * -> * where
 -- Read a variable from environment
 Var :: Index xs t -> Exp c xs t

 -- Let-binding
 -- does not need name; bound thing will be index 0, or "Here"
 Let :: Exp c           xs   t
     -> Exp c (t     ': xs)  u
     -> Exp c           xs   u

 -- Given a pre-scalar, we can get a stream of the most recent
 -- N concrete feature values.
 Recent
     :: Exp c           xs  (Pre Int)
     -> Exp c           xs  (Stream c)

 -- The most recent feature value is a post-scalar
 Last
     :: Exp c           xs  (Post c)

 -- The map worker function cannot touch any other streams or post-scalar values.
 Map :: Exp c (Pre t ': xs) (Pre u)
     -> Exp c           xs  (Stream t)
     -> Exp c           xs  (Stream u)

 -- Folds take pre-scalar functions,
 -- a stream,
 -- and return a post-scalar value
 Reduce
     :: Exp c           xs  (Pre u)
     -> Exp c (Pre t ': Pre u ': xs) (Pre u)
     -> Exp c           xs  (Stream t)
     -> Exp c           xs  (Post u)

 -- Pre-scalars can be made into post-scalars
 Cast
     :: Exp c           xs  (Pre u)
     -> Exp c           xs  (Post u)

 -- Cheating for now; use a Haskell function for scalar computations.
 -- If this takes a Pre scalar input, it will be a Pre output, etc.
 ScalarExt
     :: (GetScalar a -> b)
     -> Exp c           xs  a
     -> Exp c           xs (ReplaceScalar a b)

 -- Constant scalar
 Const
     :: a
     -> Exp c           xs (Pre a)
