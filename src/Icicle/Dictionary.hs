{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary (
    Dictionary (..)
  , Definition (..)
  , Virtual (..)
  , demographics
  ) where

import           Icicle.Data

import qualified Icicle.Common.Base            as N -- N for name
import qualified Icicle.Common.Type            as T
import qualified Icicle.Common.Exp.Exp         as X
import           Icicle.Core.Exp.Combinators
import qualified Icicle.Core.Exp.Prim        as P
import qualified Icicle.Core.Stream          as S
import qualified Icicle.Core.Reduce          as R
import qualified Icicle.Core.Program.Program as P

import           P

import           Data.Text
import qualified Data.Map                    as Map


data Dictionary =
  Dictionary [(Attribute, Definition)]
  deriving (Eq, Show)


data Definition =
    ConcreteDefinition Encoding
  | VirtualDefinition  Virtual
  deriving (Eq, Show)


data Virtual =
  Virtual {
      -- | Name of concrete attribute used as input
      concrete :: Attribute
    , program  :: P.Program Text
    } deriving (Eq, Show)



-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary 
 [ (Attribute "gender",             ConcreteDefinition StringEncoding)
 , (Attribute "age",                ConcreteDefinition IntEncoding)
 , (Attribute "state_of_residence", ConcreteDefinition StringEncoding)
 , (Attribute "salary",             ConcreteDefinition IntEncoding)
 
  -- Useless virtual features
 , (Attribute "sum of all salary",      
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_sum)

 , (Attribute "count all salary entries",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count)

 , (Attribute "mean of all salary",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_mean)

 , (Attribute "filter >= 70k; sum",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_filt_sum)

 , (Attribute "Latest 2 salary entries, unwindowed",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") (program_latest 2))

 , (Attribute "Sum of last 3000 days",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") (program_windowed_sum 3000))

 , (Attribute "Count unique",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count_unique)

 , (Attribute "Days since latest",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_days_since_latest)
 ]


-- | Dead simple sum
program_sum :: P.Program Text
program_sum
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst T.IntT (N.Name "inp"))]
 , P.reduces    = [(N.Name "red", fold_sum (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = var "red"
 }

map_fst :: T.ValType -> N.Name Text -> S.Stream Text
map_fst ty inp
 = S.STrans (S.SMap (T.PairT ty T.DateTimeT) ty) (lam (T.PairT ty T.DateTimeT) $ \p -> fstOfSource ty p) inp

fold_sum :: N.Name Text -> R.Reduce Text
fold_sum inp
 = R.RFold t t
        (lam t $ \a -> lam t $ \b -> a +~ b)
        (constI 0)
        inp
 where
  t = T.IntT


-- | Count
program_count :: P.Program Text
program_count
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "ones", S.STrans (S.SMap (T.PairT T.IntT T.DateTimeT) T.IntT) const1 (N.Name "inp"))]
 , P.reduces    = [(N.Name "count", fold_sum (N.Name "ones"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "count")
 }
 where
  const1 = lam (T.PairT T.IntT T.DateTimeT) $ \_ -> constI 1


-- | Mean salary
program_mean :: P.Program Text
program_mean
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst T.IntT (N.Name "inp"))
                  ,(N.Name "ones", S.STrans (S.SMap (T.PairT T.IntT T.DateTimeT) T.IntT) const1 (N.Name "inp"))]
 , P.reduces    = [(N.Name "count", fold_sum (N.Name "ones"))
                  ,(N.Name "sum",   fold_sum (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = var "sum" /~ var "count"
 }
 where
  const1 = lam (T.PairT T.IntT T.DateTimeT) $ \_ -> constI 1


-- | Filtered sum
program_filt_sum :: P.Program Text
program_filt_sum
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst T.IntT (N.Name "inp"))
                  ,(N.Name "filts", S.STrans (S.SFilter T.IntT) gt (N.Name "inp2"))]
 , P.reduces    = [(N.Name "sum",   fold_sum (N.Name "filts"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "sum")
 }
 where
  -- e > 70000
  gt = lam T.IntT $ \e -> e >~ constI 70000


-- | Latest N
program_latest :: Int -> P.Program Text
program_latest n
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)]
 , P.reduces    = [(N.Name "latest", R.RLatest (T.PairT T.IntT T.DateTimeT) (constI n) (N.Name "inp"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "latest")
 }

-- | Sum of last n days
program_windowed_sum :: Int -> P.Program Text
program_windowed_sum days
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.SourceWindowedDays days)
                  ,(N.Name "inp2", map_fst T.IntT (N.Name "inp"))]
 , P.reduces    = [(N.Name "sum",   fold_sum (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "sum")
 }

program_count_unique :: P.Program Text
program_count_unique
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp",  S.Source)
                  ,(N.Name "inp2", map_fst T.IntT (N.Name "inp"))]
 , P.reduces    = [(N.Name "uniq",
                        R.RFold T.IntT mT
                        (lam mT $ \acc -> lam T.IntT $ \v -> X.XPrim (P.PrimMap $ P.PrimMapInsertOrUpdate T.IntT T.IntT) @~ (lam T.IntT $ \_ -> constI 1) @~ constI 1 @~ v @~ acc)
                        (X.XValue (T.MapT T.IntT T.IntT) $ N.VMap $ Map.empty)
                        (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = [(N.Name "size", X.XPrim (P.PrimFold (P.PrimFoldMap T.IntT T.IntT) T.IntT) @~ (lam T.IntT $ \a -> lam T.IntT $ \_ -> lam T.IntT $ \b -> a +~ b) @~ constI 0 @~ var "uniq")]
 , P.returns    = var "size"
 }
 where
  mT = T.MapT T.IntT T.IntT

program_days_since_latest :: P.Program Text
program_days_since_latest
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp",  S.Source)
                  -- extract snd of pair
                  ,(N.Name "dates", S.STrans (S.SMap (T.PairT T.IntT T.DateTimeT) T.DateTimeT)
                                        (lam (T.PairT T.IntT T.DateTimeT) $ \p ->
                                           X.XPrim (P.PrimFold (P.PrimFoldPair T.IntT T.DateTimeT) T.DateTimeT)
                                        @~ (lam T.IntT $ \_ -> lam T.DateTimeT $ \b -> b )
                                        @~ p)
                                        (N.Name "inp")) ]

 , P.reduces    = [(N.Name "last", R.RLatest T.DateTimeT (constI 1) (N.Name "dates"))]

 , P.postdate   = Just (N.Name "now")
 , P.postcomps  = [(N.Name "days", X.XPrim (P.PrimFold (P.PrimFoldArray T.DateTimeT) (T.OptionT T.IntT))
                                    @~ (lam (T.OptionT T.IntT) $ \_ -> lam T.DateTimeT $ \b ->
                                            X.XPrim (P.PrimConst $ P.PrimConstSome T.IntT)
                                            @~ (X.XPrim (P.PrimDateTime P.PrimDateTimeDaysDifference) @~ b @~ var "now" ))
                                    @~ X.XValue (T.OptionT T.IntT) N.VNone @~ var "last")]

 , P.returns    = var "days"
 }

