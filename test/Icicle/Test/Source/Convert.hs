{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Query
import           Icicle.Source.ToCore.ToCore
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T

import           Icicle.Test.Source.Arbitrary
import           Icicle.Test.Core.Arbitrary ()

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Map as Map


mkElems :: Map.Map T.Variable BaseType -> Map.Map T.Variable UniverseType
mkElems = Map.map (UniverseType (Universe Elem Definitely))

mkFeatures :: Map.Map T.Variable BaseType -> Map.Map T.Variable (BaseType, a -> a)
mkFeatures = Map.map (\t -> (t, id))

prop_convert_well_typed :: BaseType -> T.Variable -> Map.Map T.Variable BaseType -> Query () T.Variable -> Property
prop_convert_well_typed tt fn f q
 = counterexample pp
 $ case typ of
    Right (qt', _)
     | restrict q
     -> let conv = freshtest $ convertQueryTop fets qt'
        in  counterexample (show conv)
          $ isRight conv
    _
     -> property Discard
 where
  qt  = QueryTop fn q
  fets = Map.singleton fn (tt, mkFeatures f)

  typ = checkQT fets qt
  pp = show $ pretty q


-- For now we can't say anything about *all* programs,
-- but we can say that a restricted subset should convert OK.
restrict
 = goQ
 where
  goQ (Query cs x)
   = all goC cs && goX x

  goC (Windowed{})
   = True
  goC (Filter{})
   = True
  goC (LetFold{})
   = True
  goC (Let _ _ x)
   = goX x
  goC _
   = False

  goX (Var{})
   = True
  goX (Prim{})
   = True
  goX (Nested () q)
   = goQ q
  goX (App _ a b)
   = goX a && goX b
  


return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { {- maxSuccess = 5000, maxSize = 10, -} maxDiscardRatio = 10000})
