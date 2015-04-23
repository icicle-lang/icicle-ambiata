-- | Type-level list junk
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Icicle.Sketch.Lists where

-- De bruijn indices.
-- Use a list of types as the environment,
-- then the index is the variable name
data Index :: [*] -> * -> * where
 Here  :: Index (x ': xs) x
 There :: Index xs x -> Index (y ': xs) x

-- Some junk for generating "stable-ish" names from de bruijn indices.
-- Basically reversing the index so it counts from end instead of front.
-- It's *so* easy to screw up, so make sure the counter and index are same list
data IxCounter :: [*] -> * where
 -- Prefix for generating names
 ICZ :: String -> IxCounter '[]
 ICS :: IxCounter xs -> IxCounter (x ': xs)

 -- Update prefix for 
 ICP :: String -> IxCounter xs -> IxCounter xs

-- Get prefix and count
precount :: IxCounter xs -> (String, Int)
precount (ICZ s) = (s, 0)
precount (ICS i)
 = let (s, c) = precount i
   in  (s, c+1)


natOfIx :: Index xs x -> Int
natOfIx Here
    = 0
natOfIx (There i)
    = 1 + natOfIx i

nameOfIx :: IxCounter xs -> Index xs x -> String
nameOfIx ici ix
 = let (p,c) = precount ici
       n    = natOfIx ix
   in p ++ show (c - n)


type family Append a b where -- :: [*] -> [*] -> [*] where
 Append '[]       ys = ys
 Append (x ': xs) ys = x ': Append xs ys
