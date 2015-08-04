{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data

import qualified Icicle.Common.Base            as N -- N for name
import qualified Icicle.Common.Type            as T
import qualified Icicle.Common.Exp.Exp         as X
import           Icicle.Core.Exp.Combinators
import qualified Icicle.Core.Exp.Prim        as P
import qualified Icicle.Common.Exp.Prim.Minimal as PM
import qualified Icicle.Core.Stream          as S
import qualified Icicle.Core.Reduce          as R
import qualified Icicle.Core.Program.Program as P

import           P

import           Data.Text
import qualified Data.Map                    as Map

import           Icicle.Dictionary.Data

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary
 [ DictionaryEntry (Attribute "gender")             (ConcreteDefinition StringEncoding)
 , DictionaryEntry (Attribute "age")                (ConcreteDefinition IntEncoding)
 , DictionaryEntry (Attribute "state_of_residence") (ConcreteDefinition StringEncoding)
 , DictionaryEntry (Attribute "salary")             (ConcreteDefinition IntEncoding)
 , DictionaryEntry (Attribute "injury")             (ConcreteDefinition $ StructEncoding
                        [StructField Mandatory (Attribute "location") StringEncoding
                        ,StructField Mandatory (Attribute "severity") IntEncoding])
  -- Useless virtual features
 , DictionaryEntry (Attribute "sum")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_sum)

 , DictionaryEntry (Attribute "count")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count)

 , DictionaryEntry (Attribute "mean")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_mean)

 , DictionaryEntry (Attribute "filter")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_filt_sum)

 , DictionaryEntry (Attribute "latest")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") (program_latest 2))

 , DictionaryEntry (Attribute "windowed")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") (program_windowed_sum 3000))

 , DictionaryEntry (Attribute "unique")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count_unique)

 , DictionaryEntry (Attribute "count_by")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count_by)


 , DictionaryEntry (Attribute "days_since")
                                    (VirtualDefinition
                                  $ Virtual (Attribute "salary") program_days_since_latest)
 ]


-- | Dead simple sum
program_sum :: P.Program Text
program_sum
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst (T.ValType T.IntT) (N.Name "inp"))]
 , P.reduces    = [(N.Name "red", fold_sum (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = var "red"
 }

map_fst :: T.ValType -> N.Name Text -> S.Stream Text
map_fst ty inp
 = S.STrans (S.SMap (T.ValType $ T.PairT ty $ T.ValType T.DateTimeT) ty) (lam (T.ValType $ T.PairT ty $ T.ValType T.DateTimeT) $ \p -> fstOfSource ty p) inp

fold_sum :: N.Name Text -> R.Reduce Text
fold_sum inp
 = R.RFold t t
        (lam t $ \a -> lam t $ \b -> a +~ b)
        (constI 0)
        inp
 where
  t = T.ValType T.IntT


-- | Count
program_count :: P.Program Text
program_count
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "ones", S.STrans (S.SMap pairIntDate (T.ValType T.IntT)) const1 (N.Name "inp"))]
 , P.reduces    = [(N.Name "count", fold_sum (N.Name "ones"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "count")
 }
 where
  const1 = lam pairIntDate $ \_ -> constI 1


-- | Mean salary
program_mean :: P.Program Text
program_mean
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst (T.ValType T.IntT) (N.Name "inp"))
                  ,(N.Name "ones", S.STrans (S.SMap pairIntDate (T.ValType T.IntT)) const1 (N.Name "inp"))]
 , P.reduces    = [(N.Name "count", fold_sum (N.Name "ones"))
                  ,(N.Name "sum",   fold_sum (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = doubleOfInt (var "sum") /~ doubleOfInt (var "count")
 }
 where
  const1 = lam pairIntDate $ \_ -> constI 1

-- | Filtered sum
program_filt_sum :: P.Program Text
program_filt_sum
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst (T.ValType T.IntT) (N.Name "inp"))
                  ,(N.Name "filts", S.STrans (S.SFilter (T.ValType T.IntT)) gt (N.Name "inp2"))]
 , P.reduces    = [(N.Name "sum",   fold_sum (N.Name "filts"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "sum")
 }
 where
  -- e > 70000
  gt = lam (T.ValType T.IntT) $ \e -> e >~ constI 70000


-- | Latest N
program_latest :: Int -> P.Program Text
program_latest n
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)]
 , P.reduces    = [(N.Name "latest", R.RLatest pairIntDate (constI n) (N.Name "inp"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "latest")
 }

-- | Sum of last n days
program_windowed_sum :: Int -> P.Program Text
program_windowed_sum days
 = P.Program
 { P.input      = T.ValType T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "inp2", map_fst (T.ValType T.IntT) (N.Name "inp"))
                  ,(N.Name "inp3", S.SWindow (T.ValType T.IntT) (constI days) Nothing (N.Name "inp2"))]
 , P.reduces    = [(N.Name "sum",   fold_sum (N.Name "inp3"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "sum")
 }

program_count_unique :: P.Program Text
program_count_unique
 = P.Program
 { P.input      = intT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp",  S.Source)
                  ,(N.Name "inp2", map_fst intT (N.Name "inp"))]
 , P.reduces    = [(N.Name "uniq",
                        R.RFold intT mT
                        (lam mT $ \acc -> lam intT $ \v -> X.XPrim (P.PrimMap $ P.PrimMapInsertOrUpdate intT intT) @~ (lam intT $ \_ -> constI 1) @~ constI 1 @~ v @~ acc)
                        (X.XValue mT $ N.VMap $ Map.empty)
                        (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = [(N.Name "size", X.XPrim (P.PrimFold (P.PrimFoldMap intT intT) intT) @~ (lam intT $ \a -> lam intT $ \_ -> lam intT $ \b -> a +~ b) @~ constI 0 @~ var "uniq")]
 , P.returns    = var "size"
 }
 where
  mT = T.ValType $ T.MapT intT intT


program_count_by :: P.Program Text
program_count_by
 = P.Program
 { P.input      = intT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp",  S.Source)
                  ,(N.Name "inp2", map_fst intT (N.Name "inp"))]
 , P.reduces    = [(N.Name "uniq",
                        R.RFold intT mT
                        (lam mT $ \acc -> lam intT $ \v -> X.XPrim (P.PrimMap $ P.PrimMapInsertOrUpdate intT intT) @~ (lam intT $ \a -> a +~ constI 1) @~ constI 1 @~ v @~ acc)
                        (X.XValue mT $ N.VMap $ Map.empty)
                        (N.Name "inp2"))]
 , P.postdate   = Nothing
 , P.postcomps  = []
 , P.returns    = var "uniq"
 }
 where
  mT = T.ValType $ T.MapT intT intT


program_days_since_latest :: P.Program Text
program_days_since_latest
 = P.Program
 { P.input      = intT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp",  S.Source)
                  -- extract snd of pair
                  ,(N.Name "dates", S.STrans (S.SMap pairIntDate (T.ValType T.DateTimeT))
                                        (lam pairIntDate $ \p ->
                                           X.XPrim (P.PrimMinimal $ PM.PrimPair $ PM.PrimPairSnd intT (T.ValType T.DateTimeT))
                                        @~ p)
                                        (N.Name "inp")) ]

 , P.reduces    = [(N.Name "last", R.RLatest (T.ValType T.DateTimeT) (constI 1) (N.Name "dates"))]

 , P.postdate   = Just (N.Name "now")
 , P.postcomps  = [(N.Name "days", X.XPrim (P.PrimFold (P.PrimFoldArray $ T.ValType T.DateTimeT) (T.ValType $ T.OptionT intT))
                                    @~ (lam (T.ValType $ T.OptionT intT) $ \_ -> lam (T.ValType T.DateTimeT) $ \b ->
                                            X.XPrim (P.PrimMinimal $ PM.PrimConst $ PM.PrimConstSome intT)
                                            @~ (X.XPrim (P.PrimMinimal $ PM.PrimDateTime PM.PrimDateTimeDaysDifference) @~ b @~ var "now" ))
                                    @~ X.XValue (T.ValType $ T.OptionT intT) N.VNone @~ var "last")]

 , P.returns    = var "days"
 }


intT :: T.ValType
intT = T.ValType T.IntT

pairIntDate :: T.ValType
pairIntDate = T.ValType $ T.PairT (T.ValType T.IntT) (T.ValType T.DateTimeT)

