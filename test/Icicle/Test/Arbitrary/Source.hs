{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Source where

import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Base (optionSmallData)
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Checker.Error
import           Icicle.Source.Query
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Common.Fresh
import qualified Icicle.Common.Base             as CB
import qualified Icicle.Common.Type             as CT

import qualified Icicle.Common.Exp.Prim.Minimal as Min
import qualified Icicle.Core.Exp.Combinators    as CE
import qualified Icicle.Core.Exp                as CE


import           Icicle.Source.ToCore.Context
import           Icicle.Source.ToCore.ToCore
import           Icicle.Source.Transform.Desugar
import           Icicle.Source.Transform.ReifyPossibility

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary.Core ()
import           Disorder.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Functor.Identity

import           Data.String   (String)

import           X.Control.Monad.Trans.Either


instance Arbitrary T.Variable where
 arbitrary
  = T.Variable <$> elements muppets

instance Arbitrary (Exp () T.Variable) where
 arbitrary
  = do tt <- arbitrary
       genExp $ TypedGenInfo Map.empty tt

instance Arbitrary Prim where
 arbitrary
  = oneof
        [ Lit . LitInt    <$> pos
        , Lit . LitDouble <$> pos'
        , Lit . LitString <$> (elements southpark)
        , Lit . LitTime   <$> arbitrary ]
    where
      -- Negative literals get parsed into negative, then a positive literal.
      -- This isn't a problem mathematically, but would break symmetry tests.
      pos  = abs <$> arbitrary
      pos' = abs <$> arbitrary

instance Arbitrary Constructor where
 arbitrary
  = oneof_vals [ ConSome, ConNone, ConTuple, ConTrue, ConFalse, ConLeft, ConRight ]

instance Arbitrary FoldType where
 arbitrary
  = oneof
        [ return FoldTypeFoldl1
        , return FoldTypeFoldl ]


instance Arbitrary (Query () T.Variable) where
 arbitrary
  = do tt <- arbitrary
       genQuery $ TypedGenInfo Map.empty tt

instance Arbitrary (QueryTop () T.Variable) where
 arbitrary
  = QueryTop <$> arbitrary <*> arbitrary <*> arbitrary

freshnamer t = counterPrefixNameState (T.Variable . Text.pack . show) (T.Variable t)

freshtest t p
 = snd <$> runFreshT p (freshnamer t)

freshcheck t
 = snd
 . flip runFresh (freshnamer t)
 . runEitherT

data QueryWithFeature
 = QueryWithFeature
 { qwfQuery     :: Query    () T.Variable
 , qwfNow       :: Maybe (CB.Name T.Variable)
 , qwfOutput    :: CB.OutputName
 , qwfFeatureT  :: CT.ValType
 , qwfFeatureN  :: CB.Name T.Variable
 , qwfTimeName  :: CB.Name T.Variable
 }
 deriving Show


qwfFeatureMap :: QueryWithFeature -> Features () T.Variable
qwfFeatureMap qwf
 = Features
    (Map.singleton (qwfFeatureN qwf) (typeOfValType (qwfFeatureT qwf), featureCtx))
     Map.empty
     (qwfNow qwf)

 where
  featureCtx
   = FeatureContext featureMap (qwfTimeName qwf)
  featureMap
   = Map.fromList
   [ (qwfFeatureN qwf, FeatureVariable (typeOfValType (qwfFeatureT qwf)) (xfst (qwfFeatureT qwf)) False)
   , (qwfTimeName qwf, FeatureVariable (typeOfValType CT.TimeT)          (xsnd (qwfFeatureT qwf)) False) ]

  xfst tt
   = CE.xApp
   $ CE.xPrim     $ CE.PrimMinimal
   $ Min.PrimPair $ Min.PrimPairFst tt CT.TimeT

  xsnd tt
   = CE.xApp
   $ CE.xPrim     $ CE.PrimMinimal
   $ Min.PrimPair $ Min.PrimPairSnd tt CT.TimeT


qwfQueryTop :: QueryWithFeature -> QueryTop () T.Variable
qwfQueryTop qwf
 = QueryTop (qwfFeatureN qwf) (qwfOutput qwf) (qwfQuery qwf)

instance Arbitrary QueryWithFeature where
 arbitrary = genQueryWithFeatureTypedGen

genQueryWithFeatureTypedGen :: Gen QueryWithFeature
genQueryWithFeatureTypedGen
 = do namebase <- elements muppets
      let make f = CB.nameOf $ CB.NameBase $ T.Variable (namebase <> f)
      let now = make "now"
      let nm  = make "featurename"
      let tm  = make "timename"
      let tgi = TypedGenInfo
              { tgiVars = Map.fromList [(now, TTAgg), (nm, TTElt), (tm, TTElt)]
              , tgiTemp = TTAgg }
      q   <- genQuery tgi

      o   <- arbitrary
      -- Note: Convert to Source type and back in generator
      -- Convert to Source type and back, because this filters out Bufs.
      -- (It actually converts them to Arrays)
      -- This is fine, because Buf can't be in feature types.
      --
      -- This was causing issues with conversion from Source, because
      -- part of the resulting Core was using the ValType with Bufs, and
      -- part was using the Source-converted type with Arrays.
      Just t <- (valTypeOfType . typeOfValType) <$> arbitrary
      return $ QueryWithFeature q (Just now) o t nm tm

genQueryWithFeatureArbitrary :: Gen QueryWithFeature
genQueryWithFeatureArbitrary
 = do namebase <- elements muppets
      let make f = CB.nameOf $ CB.NameBase $ T.Variable (namebase <> f)
      let now = make "now"
      let nm  = make "featurename"
      let tm  = make "timename"
      q   <- arbitrary
      o   <- arbitrary
      -- See "Note: Convert to Source type and back in generator" above
      Just t <- (valTypeOfType . typeOfValType) <$> arbitrary
      return $ QueryWithFeature q (Just now) o t nm tm



qwfPretty :: QueryWithFeature -> String
qwfPretty qwf
 = show $ pretty $ qwfQueryTop qwf


data CheckErr
 = CheckErrTC (CheckError () T.Variable)
 | CheckErrDS (DesugarError () T.Variable)
 deriving Show

qwfCheck :: QueryWithFeature -> Either CheckErr (QueryTop (Annot () T.Variable) T.Variable)
qwfCheck qwf
 = do qd' <- qwfDesugar qwf
      (qt',_) <- first CheckErrTC $ freshcheck "check" $ checkQT optionSmallData (qwfFeatureMap qwf) qd'
      return qt'

qwfDesugar :: QueryWithFeature -> Either CheckErr (QueryTop () T.Variable)
qwfDesugar qwf
 = first CheckErrDS
 $ runDesugar (freshnamer "desugar")
 $ desugarQT
 $ qwfQueryTop qwf

qwfConvertToCore qwf qt
 = freshtest "core" $ convertQueryTop (qwfFeatureMap qwf)
  (runIdentity $ freshtest "reify" $ reifyPossibilityQT qt)



-- Generate well-formed
type Var = CB.Name T.Variable

data TypedGenInfo
 = TypedGenInfo
 { tgiVars :: Map.Map Var TestTemporality
 , tgiTemp :: TestTemporality }

data TestTemporality
 = TTPure | TTElt | TTAgg
 deriving Eq

instance Arbitrary TestTemporality where
 arbitrary = elements [ TTPure, TTElt, TTAgg ]

availableTT :: TypedGenInfo -> TestTemporality -> Bool
availableTT tgi tt
 =  tt == TTPure
 || tt == tgiTemp tgi

genVar :: TypedGenInfo -> Gen Var
genVar tgi
 = if   Map.null found
   then arbitrary
   else oneof_vals $ Map.keys found
 where
  found
   = Map.filter (availableTT tgi)
   $ tgiVars tgi

genExp :: TypedGenInfo -> Gen (Exp () T.Variable)
genExp tgi
 = oneof_sized
    [ Var    () <$> genVar tgi
    , Prim   () <$> arbitrary ]
    [ (simplifyNestedX . Nested ()) <$> genQuery tgi
    , App    () <$> genExp tgi <*> genExp tgi
    , Case   () <$> genExp tgi
                <*> listOf1 (genCase tgi)
    , unop
    , binop
    ]

  where
   unop
    = App ()
    <$> (Prim () <$> operator_unary)
    <*> genExp tgi

   binop
    = App ()
    <$> (App ()
            <$> (Prim () <$> operator_bin)
            <*> genExp tgi)
    <*> genExp tgi

   operator_bin
    = oneof_vals
        [ Op    (ArithDouble Div)
        , Op    (ArithBinary Mul)
        , Op    (ArithBinary Add)
        , Op    (ArithBinary Sub)
        , Op    (ArithBinary Pow)
        , Op    (Relation Gt)
        , Op    (Relation Eq)
        , Op    (LogicalBinary And)
        , Op    (LogicalBinary Or)
        , Op    (TimeBinary DaysBefore)
        , Op    (TimeBinary DaysAfter)
        , Op    (TimeBinary MonthsBefore)
        , Op    (TimeBinary MonthsAfter)
        , Fun   (BuiltinTime DaysBetween)
        , Fun   (BuiltinTime DaysEpoch)
        , Fun   (BuiltinData Seq) ]

   operator_unary
    = oneof_vals
        [ Op (ArithUnary Negate)
        , Op (LogicalUnary Not)
        , Fun (BuiltinMap MapKeys)
        , Fun (BuiltinMap MapValues) ]


genCase :: TypedGenInfo -> Gen (Pattern T.Variable, Exp () T.Variable)
genCase tgi
 = do (pat,bs) <- genPat
      let tgi' = tgi { tgiVars = tgiVars tgi `Map.union` bs }
      x <- genExp tgi'
      return (pat,x)
 where
  genPat
   = oneof_sized [ return (PatDefault, Map.empty), genPatVar ]
                 [ genPatCon ]

  genPatVar
   = do a <- arbitrary
        return (PatVariable a, Map.singleton a (tgiTemp tgi))

  genPatCon
   = do con <- arbitrary
        args <- vectorOf (arityOfConstructor con) genPat
        let (args',bss) = List.unzip args
        return (PatCon con args', Map.unions bss)


genQuery :: TypedGenInfo -> Gen (Query () T.Variable)
genQuery toptgi
 = do -- Generate a very small number of contexts.
      -- It doesn't matter that the numbers are small, because they can be nested
      -- and any nesting will be simplified away
      num <- oneof_sized [return 0] [return 1]
      (ctxs,tgi') <- genCtxs num toptgi []
      x <- genExp tgi'
      return (Query (reverse ctxs) x)

 where
  genCtxs :: Int -> TypedGenInfo -> [Context () T.Variable] -> Gen ([Context () T.Variable], TypedGenInfo)
  genCtxs 0 tgi ss = return (ss, tgi)
  genCtxs n tgi ss
   = do (ctx',tgi') <- genCtx tgi
        genCtxs (n-1) tgi' (ctx' : ss)

  genCtx :: TypedGenInfo -> Gen (Context () T.Variable, TypedGenInfo)
  genCtx tgi
   | TTAgg <- tgiTemp tgi
   = genCtxAgg tgi
   | otherwise
   = genLet tgi

  genCtxAgg :: TypedGenInfo -> Gen (Context () T.Variable, TypedGenInfo)
  genCtxAgg tgi
   = oneof_sized
      [ genWindow tgi
      , nobinds tgi . Latest   () . abs <$> arbitrary ]
      [ nobinds tgi . GroupBy  () <$> genExp tgi
      , nobinds tgi . Distinct () <$> genExp tgi
      , nobinds tgi . Filter   () <$> genExp tgi
      , genFold tgi
      , genLet tgi
      , genGroupFold tgi
      ]

  genWindow tgi
   = do pre <- arbitrary
        post <- arbitrary
        return (Windowed () pre post, tgi)

  genFold tgi
   = do v <- arbitrary :: Gen Var
        f <- Fold v <$> genExp tgi { tgiTemp = TTPure } <*> genExp tgi { tgiTemp = TTElt, tgiVars = Map.insert v TTElt $ tgiVars tgi } <*> arbitrary
        let tgi' = tgi { tgiVars = Map.insert v TTAgg $ tgiVars tgi }
        return (LetFold () f, tgi')
  genLet tgi
   = do v <- arbitrary :: Gen Var
        tt' <- genTemporality tgi
        l <- Let () v <$> genExp tgi { tgiTemp = tt' }
        let tgi' = tgi { tgiVars = Map.insert v tt' $ tgiVars tgi }
        return (l,tgi')
  genGroupFold tgi
   = do k <- arbitrary :: Gen Var
        v <- arbitrary :: Gen Var
        let tgi' = tgi { tgiVars = Map.insert k TTElt $ Map.insert v TTElt $ tgiVars tgi }

        (Query ctxs x) <- genQuery tgi
        grpky <- genExp tgi
        let grp' = Query (GroupBy () grpky : ctxs) x

        let g = GroupFold () k v (Nested () grp')
        return (g, tgi')

  nobinds tgi x
   = (x, tgi)

genTemporality :: TypedGenInfo -> Gen TestTemporality
genTemporality tgi
 = case tgiTemp tgi of
    TTPure -> return TTPure
    TTElt  -> elements [TTPure, TTElt]
    TTAgg  -> elements [TTPure, TTElt, TTAgg]

