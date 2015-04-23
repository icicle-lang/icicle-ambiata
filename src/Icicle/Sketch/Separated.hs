-- | More sketching out datatypes for internal representation of programs.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Icicle.Sketch.Separated where
import Icicle.Sketch.Lists

data Map k v = Map   [(k,v)]

data Pre :: [*] -> * -> * where
            -- very silly but you get the idea
 ReadTable  :: String -- filename
            -> Pre b (Map Int String)
 PreScalar  :: Exp b a
            -> Pre b a


data Stream :: [*] -> [*] -> * -> * -> * where
 STake      :: Int
            -> Stream b s c c

 SVar       :: Index    s   t
            -> Stream b s c t

 SMap       :: Exp    (t ': b) u
            -> Stream b   s c t
            -> Stream b   s c    u
 SFilter    :: Exp    (t ': b) Bool
            -> Stream b   s c t
            -> Stream b   s c t


data Reduce :: [*] -> [*] -> * -> * where
 Reduce     :: Exp            b  a
            -> Exp (a ': t ': b) a
            -> Index  s t
            -> Reduce b s t



data TopLevel c t =
 PROGRAM (Pres '[] c t)

data Pres :: [*] -> * -> * -> * where
 LetPre     :: Pre        b    t
            -> Pres (t ': b) c u
            -> Pres       b  c u
 STREAMS    :: Streams b '[] c t
            -> Pres    b     c t

data Streams :: [*] -> [*] -> * -> * -> * where
 LetStream  :: Stream  b       s     c t
            -> Streams b (t ': s)    c u
            -> Streams b       s     c u
 REDUCES    :: Reduces b       s '[]   t
            -> Streams b       s     c t

data Reduces :: [*] -> [*] -> [*] -> * -> * where
 LetReduce  :: Reduce  b s          t
            -> Reduces b s (t ': r) u
            -> Reduces b s       r  u
 POSTS      :: Posts  (Append r b)  t
            -> Reduces b s       r  t

data Posts  :: [*] -> * -> * where
 LetPost    :: Exp         b  t
            -> Posts (t ': b) u
            -> Posts       b  u

 RETURN     :: Exp         b  t
            -> Posts       b  t


data Exp :: [*] -> * -> * where
 -- Get variable from environment
 Var        :: Index b t
            -> Exp   b t

 -- Look in map, or default
 Lookup     :: Exp b (Map k v)
            -> Exp b k
            -> Exp b v
            -> Exp b v

 -- Insert, returning new map
 Insert     :: Exp b (Map k v)
            -> Exp b k
            -> Exp b v
            -> Exp b (Map k v)

 -- Empty map
 Empty      :: Exp b (Map k v)

 -- Constant value
 Const      ::       t
            -> Exp b t

 -- Plus
 Plus       :: Exp b Int
            -> Exp b Int
            -> Exp b Int

  -- IfZero
 IfZero     :: Exp b Int
            -> Exp b t
            -> Exp b t
            -> Exp b t



example
 = PROGRAM
 -- let plans = read "plans"
 $ LetPre (ReadTable "plans")

 $ STREAMS
 -- let str   = filter (\x -> x != 0)
 $ LetStream (SFilter (IfZero (Var Here) (Const False) (Const True))
 --          $ latest 10
             $ STake 10)

 $ REDUCES
 -- let red   = fold (+) 0 str
 $ LetReduce (Reduce  (Const 0) (Plus (Var Here) (Var $ There Here)) Here)

 $ POSTS
 -- let post  = fromMaybe "default" (lookup plans red)
 $ LetPost (Lookup (Var (There Here)) (Var Here) (Const "default"))

 $ RETURN
 -- in  post
 $ Var Here

{-

pre1 = load "plans";
red1 = 0;
for (element in take 10) {
  wrk1 = (element == 0 ? true : false);
  if wrk1 {
      red1 = red1 + element;
  }
}
pos1 = pre1[red1] ?? "default";

-}

{-
prettyTop :: TopLevel c t -> String
prettyTop (PROGRAM p)
 = prettyPres (ICZ "pre") p

prettyPres :: IxCounter xs -> Pres xs c t -> String
prettyPres ici pres
 = case pres of
    
-}
