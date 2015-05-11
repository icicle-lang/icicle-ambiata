{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Core.Arbitrary where

import qualified Icicle.Internal.Pretty as PP

import           Icicle.BubbleGum
import           Icicle.Data            (AsAt(..))
import           Icicle.Data.DateTime

import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.Value

import qualified Icicle.Core.Exp                as X
import           Icicle.Core.Exp.Prim
import           Icicle.Core.Stream
import           Icicle.Core.Reduce
import           Icicle.Core.Program.Program    as P

import           Icicle.Test.Arbitrary.Base
import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           Data.Text
import qualified Data.Map   as Map


data Var
 = Var Text Int
 deriving (Eq,Ord,Show)

-- | Generate a fresh variable name that isn't mentioned elsewhere in the program,
-- (assuming that the generated program doesn't mention it)
fresh :: Int -> Name Var
fresh = Name . Var "_fresh"

-- | Check if values are equal except for functions/closures
-- Because closure heaps can differ..
equalExceptFunctions :: (Eq n, Eq p) => Value n p -> Value n p -> Bool
equalExceptFunctions p q
 | VFun{} <- p
 , VFun{} <- q
 = True
 | otherwise
 = p == q

equalExceptFunctionsE :: (Eq n, Eq p, Eq l) => Either l (Value n p) -> Either l (Value n p) -> Bool
equalExceptFunctionsE p q
 | Right p' <- p
 , Right q' <- q
 = p' `equalExceptFunctions` q'
 | otherwise
 = p == q


-- Sometimes it's nice to be able to pretty print our generated programs
instance PP.Pretty Var where
 pretty (Var t i) = PP.text (show t) <> PP.text (show i)

-- Generate totally arbitrary, totally random variables
instance Arbitrary Var where
  arbitrary =
   sized $ \size ->
    Var <$> elements viruses <*> choose (0, size)

instance Arbitrary n => Arbitrary (Name n) where
  arbitrary =
    Name <$> arbitrary

instance Arbitrary Prim where
  arbitrary =
    oneof_sized_vals
          [ PrimArith PrimArithMinus
          , PrimArith PrimArithPlus
          , PrimRelation PrimRelationGt
          , PrimRelation PrimRelationGe
          , PrimLogical  PrimLogicalNot
          , PrimLogical  PrimLogicalAnd
          ]
          [ PrimConst <$> (PrimConstPair <$> arbitrary <*> arbitrary)
          , PrimConst . PrimConstSome <$> arbitrary

          , PrimFold   PrimFoldBool <$> arbitrary
          , PrimFold <$> (PrimFoldPair <$> arbitrary <*> arbitrary) <*> arbitrary
          , PrimFold <$> (PrimFoldArray <$> arbitrary) <*> arbitrary
          , PrimFold <$> (PrimFoldOption <$> arbitrary) <*> arbitrary
          , PrimFold <$> (PrimFoldMap <$> arbitrary <*> arbitrary) <*> arbitrary

          , PrimMap <$> (PrimMapInsertOrUpdate <$> arbitrary <*> arbitrary)
          ]
          
instance Arbitrary ValType where
  arbitrary =
   -- Need to be careful about making smaller things.
   -- It's fine if they're big, but they have to fit in memory.
   oneof_sized_vals
         [ IntT
         , BoolT ]
         [ ArrayT <$> arbitrary
         , PairT  <$> arbitrary <*> arbitrary
         , MapT  <$> arbitrary <*> arbitrary
         , OptionT <$> arbitrary
         ]

-- Totally arbitrary expressions.
-- These *probably* won't type check, but sometimes you get lucky.
instance (Arbitrary n, Arbitrary p) => Arbitrary (Exp n p) where
  arbitrary =
    oneof_sized
          [ XVar  <$> arbitrary
          , XPrim <$> arbitrary
          , do  t <- arbitrary
                v <- baseValueForType t
                return $ XValue t v
          ]
          [ XApp  <$> arbitrary <*> arbitrary
          , XLam  <$> arbitrary <*> arbitrary <*> arbitrary
          , XLet  <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance Arbitrary n => Arbitrary (Stream n) where
 arbitrary =
   oneof_sized_vals
         [ Source ]
         [ STrans <$> st <*> arbitrary <*> arbitrary ]
  where
   st = oneof [ SFilter <$> arbitrary
              , SMap    <$> arbitrary <*> arbitrary ]

instance Arbitrary n => Arbitrary (Reduce n) where
 arbitrary =
   oneof [ RFold   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
         , RLatest <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary n => Arbitrary (Program n) where
 arbitrary =
   Program <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Make an effort to generate a well typed expression, but no promises
tryExpForType :: Type -> Env Var Type -> Gen (Exp Var Prim)
tryExpForType ty env
 = case ty of
    -- If ty is a first-order function, create a lambda
    FunT (FunT [] t : ts) ret
     -> do  n <- freshInEnv env
            -- Make sure the new variable is available in the environment
            let env' = Map.insert n (FunT [] t) env
            XLam n t <$> tryExpForType (FunT ts ret) env'

    -- We can't generate expressions of higher order function types: just give up
    -- This actually highlights something that should be made explicit:
    -- the types of primitives can be higher order, but expressions must be first order.
    FunT (_:_) _
     -> arbitrary

    -- We aren't generating a function, so either generate a primitive
    -- or pull something out of the context.
    -- If we can't pull something from the context, fall back to a primitive.
    -- If we can't generate a primitive, we're basically out of luck.
    FunT [] ret
     -> oneof_sized
              [ XValue ret <$> baseValueForType ret
              , context   ret ]
              [ primitive ret
              -- Because context falls back to primitive, it doesn't hurt to double
              -- its chances
              , context   ret ]

 where
  primitive r
   = do -- Try to get a primitive that has the right return type
        p <- arbitrary `suchThatMaybe` ((==r) . functionReturns . typeOfPrim)
        case p of
         -- Give up.
         -- Maybe we can generate a constant based on the type
         Nothing
          -> XValue r <$> baseValueForType r

         Just p'
          -> fillprim p'

  -- For each argument of the primitive, generate a new expression
  fillprim p
   = do as <- mapM (flip tryExpForType env) (functionArguments $ typeOfPrim p)
        return $ P.foldl XApp (XPrim p) as

  context r
   | not $ Map.null env
   = do k <- oneof (fmap return $ Map.keys env) `suchThatMaybe` ((==Just (FunT [] r)) . flip Map.lookup env)
        case k of
         -- If we can't find anything good in the context, try a primitive
         Nothing
          -> primitive r
         Just k'
          -> return $ XVar k'
   | otherwise
   = primitive r
        

-- | Generate a well typed expression.
-- If we can't generate a well typed expression we want quickcheck to count it as
-- failing to satisfy a precondition.
withTypedExp :: Testable prop => (Exp Var Prim -> ValType -> prop) -> Property
withTypedExp prop
 = forAll arbitrary $ \t ->
   forAll (tryExpForType (FunT [] t) Map.empty) $ \x ->
     checkExp0 X.coreFragment x == Right (FunT [] t) ==> prop x t
            

-- | Attempt to generate well typed expressions
-- Again, no promises.
programForStreamType :: ValType -> Gen (Program Var)
programForStreamType streamType
 = do   -- Generate a few precomputation expressions
        npres       <- choose (0,2) :: Gen Int
        (pE, pres)  <- gen_exps Map.empty npres

        -- We need at least one stream
        -- otherwise there isn't much point to the program,
        -- it'd just be a constant computation
        nstrs       <- choose (1,3) :: Gen Int
        (sE,strs)   <- gen_streams Map.empty pE nstrs

        -- And at least one reduction
        nreds       <- choose (1,3) :: Gen Int
        (rE,reds)   <- gen_reduces sE pE nreds

        -- Postcomputations with access to the reduction values
        nposts      <- choose (0,2) :: Gen Int
        (eE, posts) <- gen_exps rE nposts

        -- Finally, everything is wrapped up into one return value
        retT        <- arbitrary
        ret         <- gen_exp (FunT [] retT) eE

        return Program
               { P.input        = streamType
               , P.precomps     = pres
               , P.streams      = strs
               , P.reduces      = reds
               , P.postcomps    = posts
               , P.returns      = ret
               }

 where
  -- Generate an expression, and try very hard to make sure it's well typed
  -- (but don't try so hard that we loop forever)
  gen_exp t e
   = do x <- tryExpForType t e `suchThatMaybe` ((== Right t) . checkExp X.coreFragment e)
        case x of
         Just x' -> return x'
         Nothing -> arbitrary

  -- Generate a bunch of expressions, collecting up the environment
  gen_exps env 0
   = return (env, [])
  gen_exps env n
   = do t   <- arbitrary
        x   <- gen_exp (FunT [] t) env
        nm  <- freshInEnv env
        let env' = Map.insert nm (FunT [] t) env
        (env'', xs) <- gen_exps env' (n-1)
        return (env'', (nm, x) : xs)

  -- Generate some streams
  gen_streams :: Env Var ValType -> Env Var Type -> Int -> Gen (Env Var ValType, [(Name Var, Stream Var)])
  gen_streams sE _pE 0
   = return (sE, [])
  gen_streams sE pE n
   = do (t,str) <- gen_stream sE pE
        nm      <- freshInEnv (sE :: Env Var ValType) :: Gen (Name Var)
        (env', ss) <- gen_streams (Map.insert nm t sE) pE (n-1)
        return (env', (nm, str) : ss)
    

  -- Generate a single stream.
  -- If the stream environment is empty, we need to take from the source.
  -- If there's something in there already, we could do either
  gen_stream :: Env Var ValType -> Env Var Type -> Gen (ValType, Stream Var)
  gen_stream s_env pre_env
   | Map.null s_env
   = streamSource
   | otherwise
   = oneof [ streamSource
           , streamTransformer s_env pre_env ]

  -- Raw source or windowed
  streamSource
   = oneof [ return (streamType, Source)
           , (,) streamType . SourceWindowedDays <$> arbitrary ]

  -- Transformer: filter or map
  streamTransformer :: Env Var ValType -> Env Var Type -> Gen (ValType, Stream Var)
  streamTransformer s_env pre_env
   = do (i,t) <- oneof $ fmap return $ Map.toList s_env

        st <- oneof [ return $ SFilter t
                    , SMap t <$> arbitrary ]

        let ty = typeOfStreamTransform st
        let ot = outputOfStreamTransform st
        (,) ot <$> (STrans <$> return st <*> gen_exp ty pre_env <*> return i)

  -- Generate some reductions using given stream environment
  gen_reduces :: Env Var ValType -> Env Var Type -> Int -> Gen (Env Var Type, [(Name Var, Reduce Var)])
  gen_reduces _sE pE 0
   = return (pE, [])
  gen_reduces sE pE n
   = do (t,red) <- gen_reduce sE pE
        nm      <- freshInEnv pE
        (env', rs) <- gen_reduces sE (Map.insert nm (FunT [] t) pE) (n-1)
        return (env', (nm, red) : rs)

  -- A reduction is either a fold or a latest
  gen_reduce sE pE
   = do (i,t) <- oneof $ fmap return $ Map.toList sE
        oneof   [ do ix <- gen_exp (FunT [] IntT) pE
                     return (ArrayT t, RLatest t ix i)
                , do at <- arbitrary
                     kx <- gen_exp (FunT [FunT [] at, FunT [] t] at) pE
                     zx <- gen_exp (FunT [] at) pE
                     return (at, RFold t at kx zx i)
                ]
    


-- | Generate a new name that isn't in the environment.
-- This is fairly safe - should always succeed.
freshInEnv :: Env Var t -> Gen (Name Var)
freshInEnv env
 = arbitrary `suchThat` (not . flip Map.member env)


-- | Generate a value for given value type
baseValueForType :: ValType -> Gen BaseValue
baseValueForType t
 = case t of
    IntT
     -> VInt <$> arbitrary
    BoolT
     -> VBool <$> arbitrary
    ArrayT t'
     -> smaller (VArray <$> listOf (baseValueForType t'))
    PairT a b
     -> smaller (VPair <$> baseValueForType a <*> baseValueForType b)
    OptionT t'
     -> oneof_sized [ return VNone ]
                    [ VSome <$> baseValueForType t' ]
    MapT k v
     -> smaller
       (VMap . Map.fromList
     <$> listOf ((,) <$> baseValueForType k <*> baseValueForType v))


inputsForType :: ValType -> Gen ([AsAt (BubbleGumFact, BaseValue)], DateTime)
inputsForType t
 = sized
 $ \s -> do start   <- arbitrary
            num     <- choose (0, s)
            go num [] start
 where
  go 0 acc d
   = return (acc, dateOfDays d)
  go n acc d
   = do val <- baseValueForType t
        let d'  = dateOfDays d
        let entry = AsAt (BubbleGumFact (Flavour d d'), val) d'

        -- relatively small date increments
        diff    <- choose (0,2)
        go (n-1) (acc <> [entry]) (d+diff)
