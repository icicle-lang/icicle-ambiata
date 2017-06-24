{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Arbitrary.Source where

import           Icicle.Common.Fresh
import qualified Icicle.Common.Base             as CB
import qualified Icicle.Common.Type             as CT
import qualified Icicle.Common.Exp.Prim.Minimal as Min

import qualified Icicle.Core.Exp.Combinators    as CE
import qualified Icicle.Core.Exp                as CE
import qualified Icicle.Core                    as C

import           Icicle.Internal.Pretty
import           Icicle.Dictionary.Data
import           Icicle.Data

import           Icicle.Source.Checker.Base (optionSmallData,optionBigData)
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Checker.Error
import           Icicle.Source.Query
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Source.ToCore.Context
import           Icicle.Source.ToCore.ToCore
import qualified Icicle.Source.ToCore.Base as S
import           Icicle.Source.Transform.Desugar
import           Icicle.Source.Transform.ReifyPossibility

import           Icicle.Compiler.Source

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary.Core (testFresh)

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


-- Fresh name helpers ---------------
--
freshnamer t = counterPrefixNameState (T.Variable . Text.pack . show) (T.Variable t)

freshtest t p
 = snd <$> runFreshT p (freshnamer t)

freshcheck t
 = snd
 . flip runFresh (freshnamer t)
 . runEitherT


-- Query and related typechecker options --------
-- All the stuff needed to typecheck, convert a query and so on
data QueryWithFeature
 = QueryWithFeature
 { qwfQuery     :: Query () T.Variable
 , qwfNow       :: Maybe (CB.Name T.Variable)
 , qwfOutput    :: OutputId
 , qwfFeatureT  :: CT.ValType
 , qwfFeatureN  :: CB.Name T.Variable
 , qwfFeatureId :: InputId
 , qwfTimeName  :: CB.Name T.Variable
 , qwfFeatureK  :: InputKey () T.Variable
 }

instance Show QueryWithFeature where
  show (QueryWithFeature q now out ty name iid time key)
    =  "QueryWithFeature:"
    <> "\n  Query: " <> show (pretty q)
    <> "\n  Now: " <> show (pretty now)
    <> "\n  Output: " <> show (pretty out)
    <> "\n  Type: " <> show (pretty ty)
    <> "\n  Name: " <> show (pretty name)
    <> "\n  Input: " <> show (pretty iid)
    <> "\n  Time: " <> show (pretty time)
    <> "\n  Key: " <> show (pretty $ unInputKey key)

-- | Generate feature map for typechecking and conversion to Core.
qwfFeatureMap :: QueryWithFeature -> Features () T.Variable (InputKey () T.Variable)
qwfFeatureMap qwf
 = Features
    (Map.singleton (qwfFeatureId qwf) featureCrt)
     Map.empty
    (qwfNow qwf)

 where
  featureCrt
   = FeatureConcrete (qwfFeatureK qwf) (typeOfValType (qwfFeatureT qwf)) featureCtx
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
 = QueryTop (QualifiedInput $ qwfFeatureId qwf) (qwfOutput qwf) (qwfQuery qwf)

-- | Try to generate a well-typed query.
-- The argument defines how likely it is to "table flip" and generate crazy expressions.
-- The crazy expressions are less likely to typecheck, but may cover more corner cases.
genQueryWithFeatureTypedGen :: Int -> Gen QueryWithFeature
genQueryWithFeatureTypedGen tableflipPercent
 = do namebase <- elements muppets
      let make f = CB.nameOf $ CB.NameBase $ T.Variable (namebase <> f)
      let now = make "now"
      let nm  = make "featurename"
      let iid = [inputid|ns:id|]
      let tm  = make "timename"
      let tgi = TypedGenInfo
              { tgiVars = Map.fromList [(now, TTAgg), (nm, TTElt), (tm, TTElt)]
              , tgiTemp = TTAgg
              , tgiTableflipPercent = tableflipPercent }
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
      k      <- genQueryKey t
      return $ QueryWithFeature q (Just now) o t nm iid tm k

-- | Use arbitrary instance to generate query.
-- Less likely to typecheck, but more likely to cover crazy corner cases.
genQueryWithFeatureArbitrary :: Gen QueryWithFeature
genQueryWithFeatureArbitrary
 = do namebase <- elements muppets
      let make f = CB.nameOf $ CB.NameBase $ T.Variable (namebase <> f)
      let now = make "now"
      let nm  = make "namespace:featurename"
      let Just iid = parseInputId (namebase <> "namespace:featurename")
      let tm  = make "timename"
      q   <- arbitrary
      o   <- arbitrary
      -- See "Note: Convert to Source type and back in generator" above
      Just t <- (valTypeOfType . typeOfValType) <$> arbitrary
      k      <- genQueryKey t
      return $ QueryWithFeature q (Just now) o t nm iid tm k


-- | Pretty-print the query and stuff
qwfPretty :: QueryWithFeature -> String
qwfPretty qwf
 = show $ pretty $ qwfQueryTop qwf


data CheckErr
 = CheckErrTC (CheckError () T.Variable)
 | CheckErrDS (DesugarError () T.Variable)
 | CheckErrImpossible
 deriving Show

qwfCheckKey :: QueryWithFeature -> Either CheckErr (InputKey (Annot () T.Variable) T.Variable)
qwfCheckKey qwf = case unInputKey (qwfFeatureK qwf) of
  Nothing -> return $ InputKey Nothing
  Just k  -> do
    let q = QueryTop (QualifiedInput $ qwfFeatureId qwf)
                     [outputid|dummy_nsp:dummy_output|]
                     (Query [Distinct () k] (Prim () (Lit (LitInt 0))))

    (checked, _) <- runIdentity
                  . freshtest "core_key"
                  . runEitherT $ do
                      q' <- hoistEither $ first CheckErrDS $ runDesugar (freshNamer "dummy_desugar") (desugarQT q)
                      firstEitherT CheckErrTC $ checkQT defaultCheckOptions (qwfFeatureMap qwf) q'

    case contexts . query $ checked of
      Distinct _ xx' : _
        -> Right . InputKey . Just $ xx'
      _ -> Left CheckErrImpossible

qwfCheck :: QueryWithFeature -> Either CheckErr (QueryTop (Annot () T.Variable) T.Variable )
qwfCheck qwf
 = do qd' <- qwfDesugar qwf
      (qt',_) <- first CheckErrTC
               $ freshcheck "check"
               $ checkQT optionSmallData (qwfFeatureMap qwf) qd'
      return qt'

qwfCheckBigData :: QueryWithFeature -> Either CheckErr (QueryTop (Annot () T.Variable) T.Variable)
qwfCheckBigData qwf
 = do qd' <- qwfDesugar qwf
      (qt',_) <- first CheckErrTC $ freshcheck "check" $ checkQT optionBigData (qwfFeatureMap qwf) qd'
      return qt'

qwfDesugar :: QueryWithFeature -> Either CheckErr (QueryTop () T.Variable)
qwfDesugar qwf
 = first CheckErrDS
 $ runDesugar (freshnamer "desugar")
 $ desugarQT
 $ qwfQueryTop qwf

qwfConvertToCore :: QueryWithFeature
                 -> InputKey (Annot () T.Variable) T.Variable
                 -> QueryTop    (Annot () T.Variable) T.Variable
                 -> Either (S.ConvertError () T.Variable) (C.Program () T.Variable)
qwfConvertToCore qwf key qt
 = freshtest "core"
 $ convertQueryTop
     (replaceKey key $ qwfFeatureMap qwf)
     (runIdentity $ freshtest "reify" $ reifyPossibilityQT qt)
 where
  replaceKey x f
    = f { featuresConcretes = fmap (\c -> c { featureConcreteKey = x }) $ featuresConcretes f }



-- Generating well-formed queries ---------
-- Actually generating well-typed queries is going to be very hard, but we can
-- at least make a token effort.
-- We keep track of the bound variables, and whether they are available as
-- pure expressions, element, or aggregates.
-- Depending on the sort of expression we want to create, we can choose only that
-- sort of variable.

-- | State of generator
data TypedGenInfo
 = TypedGenInfo {
   -- | Variables that are currently bound
   tgiVars :: Map.Map (CB.Name T.Variable) TestTemporality,
   -- | The sort of expression to try to produce
   tgiTemp :: TestTemporality,
   -- | How likely we are to generate a crazy expression
   tgiTableflipPercent :: Int
 }

-- | By default, why not tableflip 30% of the time?
-- Seems high, but also seems fast enough
tgiDefaultTableflip :: Int
tgiDefaultTableflip = 30

-- | Decide whether to tableflip: run second generator if so, otherwise first generator
genTableflip :: TypedGenInfo -> Gen a -> Gen a -> Gen a
genTableflip tgi good bad
 = do i <- choose (0,100)
      if i < tgiTableflipPercent tgi then bad else good

-- | Temporality information with nothing else attached
data TestTemporality
 = TTPure | TTElt | TTAgg
 deriving Eq

instance Arbitrary TestTemporality where
 arbitrary = elements [ TTPure, TTElt, TTAgg ]

-- | Check if a variable is available for expression we are trying to generate.
-- Pure expressions are available anywhere
availableTT :: TypedGenInfo -> TestTemporality -> Bool
availableTT tgi tt
 =  tt == TTPure
 || tt == tgiTemp tgi

genVar :: TypedGenInfo -> Gen (CB.Name T.Variable)
genVar tgi
 = genTableflip tgi goodvar arbitrary
 where
  goodvar
   = if   Map.null found
     then arbitrary
     else oneof_vals $ Map.keys found
  found
   = Map.filter (availableTT tgi)
   $ tgiVars tgi

-- | Generate an expression
genExp :: TypedGenInfo -> Gen (Exp () T.Variable)
genExp tgi
 = genTableflip tgi good bad
 where
  -- Good case, we just want:
  good
   = oneof_sized
      -- in-scope variables
      [ Var    () <$> genVar tgi
      -- literal primitives only
      , Prim   () <$> genLitPrim ]
      -- sub-queries
      [ (simplifyNestedX . Nested ()) <$> genQuery tgi
      -- case statements
      , Case   () <$> genExp tgi
                  <*> listOf1 (genCase tgi)
      -- well-formed applications to primitives
      , primApps
      ]

  -- In the bad case:
  bad
   = oneof_sized
      -- Unapplied primitives!
      [ Prim   () <$> arbitrary ]
      -- Applications whose lhs might not be a function/primitive
      [ App    () <$> genExp tgi <*> genExp tgi
      -- Randomly changing the temporality
      , arbitrary >>= \tt -> genExp tgi { tgiTemp = tt }
      ]

  primApps
   = do p <- arbitrary
        let ft = testFresh "" $ primLookup' p
        let num = length $ functionArguments ft
        xs <- vectorOf num (genExp tgi)
        return $ foldl (App ()) (Prim () p) xs


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

genQueryKey :: CT.ValType -> Gen (InputKey () T.Variable)
genQueryKey t
  = frequency [ (1, pure (InputKey Nothing)), (10, InputKey . Just <$> genKeyExp) ]
 where
  var n = (CB.nameOf $ CB.NameBase n, TTElt)
  genKeyExp = genExp . tgi $ case t of
    CT.StructT ts
      ->   var "fields" : fmap (var . T.Variable . CT.nameOfStructField) (Map.keys $ CT.getStructType ts)
    _ -> [ var "value" ]

  tgi stuff = TypedGenInfo
   (Map.fromList $ stuff <> [(CB.nameOf (CB.NameBase "time"), TTPure)])
   TTElt
   tgiDefaultTableflip

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
      [ nobinds tgi . GroupBy  () <$> genExp tgi { tgiTemp = TTElt}
      , nobinds tgi . Distinct () <$> genExp tgi { tgiTemp = TTElt}
      , nobinds tgi . Filter   () <$> genExp tgi { tgiTemp = TTElt}
      , genFold tgi
      , genLet tgi
      , genGroupFold tgi
      ]

  genWindow tgi
   = do pre <- arbitrary
        post <- arbitrary
        return (Windowed () pre post, tgi)

  genFold tgi
   = do v <- arbitrary :: Gen (CB.Name T.Variable)
        f <- Fold v <$> genExp tgi { tgiTemp = TTPure } <*> genExp tgi { tgiTemp = TTElt, tgiVars = Map.insert v TTElt $ tgiVars tgi } <*> arbitrary
        let tgi' = tgi { tgiVars = Map.insert v TTAgg $ tgiVars tgi }
        return (LetFold () f, tgi')
  genLet tgi
   = do v <- arbitrary :: Gen (CB.Name T.Variable)
        tt' <- genTemporality tgi
        l <- Let () v <$> genExp tgi { tgiTemp = tt' }
        let tgi' = tgi { tgiVars = Map.insert v tt' $ tgiVars tgi }
        return (l,tgi')
  genGroupFold tgi
   = do k <- arbitrary :: Gen (CB.Name T.Variable)
        v <- arbitrary :: Gen (CB.Name T.Variable)
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


genLitPrim :: Gen Prim
genLitPrim
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


-- Boring arbitrary instances ---
-- These are all quite simple, and just use the above functions when they aren't

instance Arbitrary T.Variable where
 arbitrary
  = T.Variable <$> elements muppets

instance Arbitrary (Exp () T.Variable) where
 arbitrary
  = do tt <- arbitrary
       genExp $ TypedGenInfo Map.empty tt tgiDefaultTableflip

instance Arbitrary Prim where
 arbitrary
  = oneof
      [ oneof_vals ops
      , oneof_vals funs
      , oneof_vals cons
      , genLitPrim ]

  where
   ops
    = fmap Op
        [ ArithUnary Negate
        , ArithBinary Mul
        , ArithBinary Add
        , ArithBinary Sub
        , ArithBinary Pow
        , ArithDouble Div
        , Relation Lt
        , Relation Le
        , Relation Gt
        , Relation Ge
        , Relation Eq
        , Relation Ne
        , LogicalUnary Not
        , LogicalBinary And
        , LogicalBinary Or
        , TimeBinary DaysBefore
        , TimeBinary DaysAfter
        , TimeBinary WeeksBefore
        , TimeBinary WeeksAfter
        , TimeBinary MonthsBefore
        , TimeBinary MonthsAfter
        , TupleComma
        ]

   funs
    = fmap Fun
        [ BuiltinMath  Log
        , BuiltinMath  Exp
        , BuiltinMath  Sqrt
        , BuiltinMath  ToDouble
        , BuiltinMath  Abs
        , BuiltinMath  Floor
        , BuiltinMath  Ceiling
        , BuiltinMath  Round
        , BuiltinMath  Truncate
        , BuiltinTime  DaysBetween
        , BuiltinTime  DaysJulianEpoch
        , BuiltinTime  SecondsBetween
        , BuiltinTime  SecondsJulianEpoch
        , BuiltinData  Seq
        , BuiltinData  Box
        , BuiltinMap   MapKeys
        , BuiltinMap   MapValues
        , BuiltinMap   MapCreate
        , BuiltinMap   MapInsert
        , BuiltinMap   MapDelete
        , BuiltinMap   MapLookup
        , BuiltinArray ArraySort ]

   cons
    = fmap PrimCon
    [ ConSome, ConNone, ConTrue, ConFalse, ConLeft, ConRight ]


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
       genQuery $ TypedGenInfo Map.empty tt tgiDefaultTableflip

instance Arbitrary (QueryTop () T.Variable) where
 arbitrary
  = QueryTop <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QueryWithFeature where
 arbitrary = genQueryWithFeatureTypedGen tgiDefaultTableflip

