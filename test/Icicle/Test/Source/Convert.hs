{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Query
import           Icicle.Source.ToCore.Context
import           Icicle.Source.ToCore.ToCore
import           Icicle.Source.Type
import           Icicle.Source.Transform.Desugar
import qualified Icicle.Source.Lexer.Token as T

import qualified Icicle.Common.Exp.Prim.Minimal as Min
import qualified Icicle.Core.Exp.Combinators    as CE
import qualified Icicle.Core.Exp                as CE
import qualified Icicle.Common.Base             as CB
import qualified Icicle.Common.Type             as CT

import qualified Icicle.Core.Program.Check      as CCheck

import           Icicle.Test.Source.Arbitrary
import           Icicle.Test.Core.Arbitrary ()

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Map as Map

prop_convert_ok :: CB.OutputName -> CT.ValType -> CB.Name T.Variable -> Query () T.Variable -> Property
prop_convert_ok nm tt fn q
 = let original = QueryTop fn nm q
       desugar  = runDesugar freshnamer $ desugarQT original
   in  counterexample (show (pretty original))
     $ case desugar of
        Left _
         -> property Discard
        Right qt
         -> let typ = (freshcheck . checkQT fets) qt
            in  case typ of
                 Left _
                  -> property Discard
                 Right (qt', _)
                  -> let conv = freshtest (convertQueryTop fets qt')
                     in  counterexample (show conv) $ isRight conv
 where
  fets = Features
        (Map.singleton fn (typeOfValType tt, Map.singleton fn (typeOfValType tt, xfst tt)))
         Map.empty

prop_convert_is_well_typed :: CB.OutputName -> CB.Name T.Variable -> Query () T.Variable -> Property
prop_convert_is_well_typed nm fn q
 = counterexample pp
 $ case typ of
    Right (qt', _)
     | restrict q
     , Right c' <- freshtest $ convertQueryTop fets qt'
     , check    <- CCheck.checkProgram c'
     -> counterexample (show $ pretty c')
      $ counterexample (show check)
      $ isRight check
    _
     -> property Discard
 where
  qt  = QueryTop fn nm q

  fets :: Features () T.Variable
  fets = Features
       (Map.singleton fn (typeOfValType tt, Map.singleton fn (typeOfValType tt, xfst tt)))
        Map.empty

  tt = CT.IntT

  typ = freshcheck $ checkQT fets qt
  pp = show $ pretty q


-- For now we can't say anything about *all* programs,
-- but we can say that a restricted subset should convert OK.
restrict
 = const True
 where
 {-
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
   -}
  
xfst tt
 = CE.xApp
 $ CE.xPrim $ CE.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst tt CT.DateTimeT



return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5000, maxSize = 10, maxDiscardRatio = 10000})
