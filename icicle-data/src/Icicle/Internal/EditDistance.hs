{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Internal.EditDistance (
    editDistance
    ) where

import           P
import qualified Data.Vector.Unboxed.Mutable as Vector
import           Icicle.Internal.Pretty

import           Control.Monad.ST (runST, ST)
import qualified Data.Map as Map
import           Data.List ((!!))

editDistance :: Doc -> Doc -> Int
editDistance a b =
  let pretty_comp = (flip displayS "") . renderCompact . pretty
      a' = pretty_comp a
      b' = pretty_comp b
  in damerauLevenshtein a' b'

-- This function it from the library language-spelling, but it has dependencies on
-- ListLike, listlike instances and a version of Data.Text which clashes with what
-- we're using. For just one function, it's not too terrible to just bring it into
-- this file.

damerauLevenshtein :: (Ord a) => [a] -> [a] -> Int
damerauLevenshtein s1 s2
    | s1 == [] = length s2
    | s2 == [] = length s1
    | otherwise = runST $ do v <- Vector.replicate ((len1 + 2) * (len2 + 2)) 0
                             let inf = (len1 + len2)
                             Vector.write v 0 inf
                             mapM_ (\r -> Vector.write v (ix (r + 1) 0) inf >>
                                          Vector.write v (ix (r + 1) 1) r) [0..len1]
                             mapM_ (\c -> Vector.write v (ix 0 (c + 1)) inf >>
                                          Vector.write v (ix 1 (c + 1)) c) [0..len2]
                             foldM_ (go1 v) chPos' [1..len1]
                             vlast v
  where
    len1 = length s1
    len2 = length s2
    ix r c = r * (len2 + 2) + c
    chPos' = foldr (\ch -> Map.insert ch 0) Map.empty (s1 <> s2)
    go1 v chPos r = do foldM_ (go2 v chPos r) 0 [1..len2]
                       return (Map.insert (s1 !! (r - 1)) r chPos)
    go2 v chPos r db c =
        let ch1 = s1 !! (r - 1)
            ch2 = s2 !! (c - 1)
            r1 = chPos Map.! ch2
            c1 = db
        in do d1 <- Vector.read v (ix r c)
              (d2, db') <- if ch1 == ch2 then return (d1, c)
                           else do d3 <- Vector.read v (ix (r + 1) c)
                                   d4 <- Vector.read v (ix r (c + 1))
                                   return (min3 d1 d3 d4 + 1, db)
              d5 <- (\n -> n + (r - r1 - 1) + 1 + (c - c1 - 1)) <$>
                    Vector.read v (ix r1 c1)
              Vector.write v (ix (r + 1) (c + 1)) (min d2 d5)
              return db'

    min3 :: Ord a => a -> a -> a -> a
    min3 x y z = min x (min y z)

    vlast :: Vector.Unbox a => Vector.STVector s a -> ST s a
    vlast v = Vector.read v (Vector.length v - 1)
