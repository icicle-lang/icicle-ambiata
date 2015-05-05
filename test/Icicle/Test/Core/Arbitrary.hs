{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Core.Arbitrary where

import qualified Icicle.Internal.Pretty as PP

import           Icicle.Core.Base
import           Icicle.Core.Exp
import           Icicle.Core.Type
import           Icicle.Core.Eval.Exp

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
equalExceptFunctions :: Eq n => Value n -> Value n -> Bool
equalExceptFunctions p q
 | VFun{} <- p
 , VFun{} <- q
 = True
 | otherwise
 = p == q

equalExceptFunctionsE :: (Eq n, Eq l) => Either l (Value n) -> Either l (Value n) -> Bool
equalExceptFunctionsE p q
 | Right p' <- p
 , Right q' <- q
 = p' `equalExceptFunctions` q'
 | otherwise
 = p == q


instance PP.Pretty Var where
 pretty (Var t i) = PP.text (show t) <> PP.text (show i)

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
          , PrimConst (PrimConstInt 0)
          , PrimConst (PrimConstInt 1)
          , PrimConst (PrimConstInt 2)
          , PrimConst (PrimConstBool True)
          , PrimConst (PrimConstBool False)
          ]
          [ PrimConst <$> PrimConstArrayEmpty <$> arbitrary
          , PrimConst <$> (PrimConstPair <$> arbitrary <*> arbitrary)
          , PrimConst . PrimConstSome <$> arbitrary
          , PrimConst . PrimConstNone <$> arbitrary
          , PrimConst <$> (PrimConstMapEmpty <$> arbitrary <*> arbitrary)

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

instance Arbitrary n => Arbitrary (Exp n) where
  arbitrary =
    oneof_sized
          [ XVar  <$> arbitrary
          , XPrim <$> arbitrary ]
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
              , SMap    <$> arbitrary <*> arbitrary
              , STake   <$> arbitrary ]

instance Arbitrary n => Arbitrary (Reduce n) where
 arbitrary =
   oneof [ RFold   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
         , RLatest <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary n => Arbitrary (Program n) where
 arbitrary =
   Program <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Make an effort to generate a well typed expression, but no promises
tryExpForType :: Type -> Env Var Type -> Gen (Exp Var)
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
     -> oneof [ primitive ret
              -- Because context falls back to primitive, it doesn't hurt to double
              -- its chances
              , context   ret
              , context   ret ]

 where
  primitive r
   = do -- Try to get a primitive that has the right return type
        p <- arbitrary `suchThatMaybe` ((==r) . functionReturns . typeOfPrim)
        case p of
         -- Give up.
         -- Maybe we can generate a constant based on the type
         Nothing
          -> case r of
              IntT     -> fillprim $ PrimConst (PrimConstInt 0)
              BoolT    -> fillprim $ PrimConst (PrimConstBool False)
              ArrayT i -> fillprim $ PrimConst (PrimConstArrayEmpty i)
              MapT k v -> fillprim $ PrimConst (PrimConstMapEmpty k v)
              OptionT i -> fillprim $ PrimConst (PrimConstNone i)
              PairT a b -> fillprim $ PrimConst (PrimConstPair a b)

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
withTypedExp :: Testable prop => (Exp Var -> ValType -> prop) -> Property
withTypedExp prop
 = forAll arbitrary $ \t ->
   forAll (tryExpForType (FunT [] t) Map.empty) $ \x ->
     checkExp0 x == Right (FunT [] t) ==> prop x t
            

-- | Attempt to generate well typed expressions
-- Again, no promises.
programForStreamType :: ValType -> Gen (Program Var)
programForStreamType streamType
 = do   -- generate pres
        npres       <- choose (0,2) :: Gen Int
        (pE, pres)  <- gen_exps Map.empty npres

        nstrs       <- choose (1,3) :: Gen Int
        (sE,strs)   <- gen_streams Map.empty pE nstrs

        nreds       <- choose (1,3) :: Gen Int
        (rE,reds)   <- gen_reduces sE pE nreds

        nposts      <- choose (0,2) :: Gen Int
        (eE, posts) <- gen_exps rE nposts

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
  gen_exp t e
   = do x <- tryExpForType t e `suchThatMaybe` ((== Right t) . checkExp e)
        -- (but don't try so hard that we loop forever)
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

  gen_streams :: Env Var ValType -> Env Var Type -> Int -> Gen (Env Var ValType, [(Name Var, Stream Var)])
  gen_streams sE _pE 0
   = return (sE, [])
  gen_streams sE pE n
   = do (t,str) <- stream sE pE
        nm      <- freshInEnv (sE :: Env Var ValType) :: Gen (Name Var)
        (env', ss) <- gen_streams (Map.insert nm t sE) pE (n-1)
        return (env', (nm, str) : ss)
    

  stream :: Env Var ValType -> Env Var Type -> Gen (ValType, Stream Var)
  stream s_env pre_env
   | Map.null s_env
   = streamSource
   | otherwise
   = oneof [ streamSource
           , streamTransformer s_env pre_env ]

  streamSource
   = oneof [ return (streamType, Source)
           , (,) streamType . SourceWindowedDays <$> arbitrary ]

  streamTransformer :: Env Var ValType -> Env Var Type -> Gen (ValType, Stream Var)
  streamTransformer s_env pre_env
   = do (i,t) <- oneof $ fmap return $ Map.toList s_env

        st <- oneof [ return $ SFilter t
                    , SMap t <$> arbitrary ]

        let ty = typeOfStreamTransform st
        let ot = outputOfStreamTransform st
        (,) ot <$> (STrans <$> return st <*> gen_exp ty pre_env <*> return i)
        
  gen_reduces :: Env Var ValType -> Env Var Type -> Int -> Gen (Env Var Type, [(Name Var, Reduce Var)])
  gen_reduces _sE pE 0
   = return (pE, [])
  gen_reduces sE pE n
   = do (t,red) <- gen_reduce sE pE
        nm      <- freshInEnv pE
        (env', rs) <- gen_reduces sE (Map.insert nm (FunT [] t) pE) (n-1)
        return (env', (nm, red) : rs)

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
