{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary (
    Dictionary (..)
  , Definition (..)
  , Virtual (..)
  , demographics
  ) where

import           Icicle.Data

import qualified Icicle.Core.Base            as N -- N for name
import qualified Icicle.Core.Type            as T
import qualified Icicle.Core.Exp             as X
import           Icicle.Core.Exp.Combinators
import qualified Icicle.Core.Stream          as S
import qualified Icicle.Core.Reduce          as R
import qualified Icicle.Core.Program.Program as P

import           P

import           Data.Text


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
 , (Attribute "sum_salary",         VirtualDefinition
                                  $ Virtual (Attribute "salary") program_sum)
 , (Attribute "num_salary",         VirtualDefinition
                                  $ Virtual (Attribute "salary") program_count)
 , (Attribute "sum_salary_above_70k",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") program_filt_sum)
 , (Attribute "latest2",
                                    VirtualDefinition
                                  $ Virtual (Attribute "salary") (program_latest 2))
 ]


-- | Dead simple sum
program_sum :: P.Program Text
program_sum
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)]
 , P.reduces    = [(N.Name "red", fold_sum (N.Name "inp"))]
 , P.postcomps  = []
 , P.returns    = var "red"
 }

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
                  ,(N.Name "ones", S.STrans (S.SMap T.IntT T.IntT) const1 (N.Name "inp"))]
 , P.reduces    = [(N.Name "count", fold_sum (N.Name "ones"))]
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "count")
 }
 where
  const1 = lam T.IntT $ \_ -> constI 1


-- | Filtered sum
program_filt_sum :: P.Program Text
program_filt_sum
 = P.Program
 { P.input      = T.IntT
 , P.precomps   = []
 , P.streams    = [(N.Name "inp", S.Source)
                  ,(N.Name "filts", S.STrans (S.SFilter T.IntT) gt (N.Name "inp"))]
 , P.reduces    = [(N.Name "sum",   fold_sum (N.Name "filts"))]
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
 , P.reduces    = [(N.Name "latest", R.RLatest T.IntT (constI n) (N.Name "inp"))]
 , P.postcomps  = []
 , P.returns    = X.XVar (N.Name "latest")
 }
