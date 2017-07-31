{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Test.Gen.Core.Program where

import qualified Icicle.Core.Exp.Simp as Simp
import qualified Data.Functor.Identity as Identity
import qualified Icicle.Common.FixT as Fix

import           Icicle.Core.Exp.Prim
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Common.Exp
import           Disorder.Corpus
import           Icicle.Test.Arbitrary.Data ()
import           Icicle.Core.Exp.Combinators
import           Icicle.Core.Stream
import           Icicle.Core.Program.Program

import Icicle.Test.Gen.Core.Prim
import Icicle.Test.Gen.Core.Type
import Icicle.Test.Gen.Core.Value
import qualified Icicle.Common.Exp.Prim.Minimal as PM

import           Icicle.Test.Arbitrary.Data

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen.QuickCheck as Qc

import P
import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Prelude

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State

newtype Priority = Priority { getPriority :: Int }

type GenT' = State.StateT Int (Reader.ReaderT (Env Var (ValType,Priority), Map.Map ValType [Prim]) Gen)
type C m = (Reader.MonadReader (Env Var (ValType,Priority), Map.Map ValType [Prim]) m, State.MonadState Int m, MonadGen m, MonadPlus m)

freshName :: C m => m (Name Var)
freshName = Gen.element viruses >>= freshName'

freshName' :: C m => Text -> m (Name Var)
freshName' n = do
  i <- State.get
  State.put (i + 1)
  return $ nameOf $ NameBase $ Var n i


freshBind :: C m => ValType -> Priority -> (Name Var -> m a) -> m a
freshBind t pri m = do
  v <- freshName
  Reader.local (\(e,p) -> (Map.insert v (t,pri) e, p)) (m v)

runCoreGen :: ValType -> GenT' a -> Gen a
runCoreGen t m = Gen.sized $ \s -> Gen.resize (sqrt' s) $ do
  primmap <- Gen.lift $ genPrimLookupList $ genDerivedTypeTop t
  Reader.runReaderT (State.evalStateT m 0) (Map.empty, primmap)
 where
  sqrt' = truncate . (sqrt :: Double -> Double) . fromIntegral

genExpTop :: Gen (Exp () Var Prim, ValType)
genExpTop = do
 t <- genInputType
 runCoreGen t genExp

genExpForTypeTop :: ValType -> Gen (Exp () Var Prim)
genExpForTypeTop t = do
 runCoreGen t $ genExpForType (FunT [] t)


-- TODO: we should include the outputType in runCoreGen's primitive map generation
programForStreamType :: ValType -> ValType -> Gen (Program () Var)
programForStreamType streamType outputType = runCoreGen streamType (programForStreamType' streamType outputType)

programForStreamType' :: C m => ValType -> ValType -> m (Program () Var)
programForStreamType' streamType outputType = do
  nmapsz <- freshName' "maxMapSize"
  nid    <- freshName' "id"
  ntime  <- freshName' "factTime"
  ndate  <- freshName' "snapshotTime"
  ninput <- freshName' "input"

  let env0 = Map.fromList  [ ( ndate,  (TimeT, Priority 20))
                           , ( nmapsz, (IntT,  Priority 1)) ]

  -- Generate a few precomputation expressions
  npres       <- genCount
  (envP',pres)<- genExps env0 npres

  let envS0 = Map.fromList [ ( ninput, (PairT streamType TimeT, Priority 40))
                           , ( nid,    (FactIdentifierT, Priority 1))
                           , ( ntime,  (TimeT, Priority 20)) ]

  nstrs       <- genCount
  (envS',strs)<- genStreams envP' envS0 nstrs

  -- Postcomputations with access to the reduction values
  nposts         <- genCount
  (envE', posts) <- genExps envS' nposts

  -- Finally, everything is wrapped up into one return value
  retName     <- Qc.arbitrary
  ret         <- genExpForTypeEnv (FunT [] outputType) envE'

  return Program
         { inputType    = streamType
         , factValName  = ninput
         , factIdName   = nid
         , factTimeName = ntime
         , snaptimeName = ndate
         , maxMapSize   = nmapsz
         , precomps     = pres
         , streams      = strs
         , postcomps    = posts
         , returns      = [(retName, ret)]
         }

 where
  genCount = Gen.integral $ Range.linear 1 (30 :: Int)

  stuffEnv e m = Reader.local (\(_,p) -> (e,p)) m

  genExpEnv e = stuffEnv e $ genExp

  genExpForTypeEnv t e = stuffEnv e $ genExpForType t

  -- Generate a bunch of expressions, collecting up the environment
  genExps env 0
   = return (env, [])
  genExps env n
   = do (x,t)       <- genExpEnv env
        nm          <- freshName
        let env'     = Map.insert nm (t, Priority 20) env
        (env'', xs) <- genExps env' (n-1)
        return (env'', (nm, x) : xs)

  -- Generate some streams
  genStreams zE _ 0
   = return (zE, [])
  genStreams zE kE n
   = do (zE',s')    <- Gen.recursive Gen.choice [genFold zE kE] [genFilter zE kE (n-1)]
        (zE'', ss) <- genStreams zE' kE (n-1)
        return (zE'', s' : ss)

  genFold zE kE
   = do n     <- freshName
        (z,t) <- genExpEnv zE
        let zE' = Map.insert n (t, Priority 50) zE
        k     <- genExpForTypeEnv (FunT [] t) (Map.union zE' kE)
        return (zE', SFold n t z k) 

  genFilter zE kE num
   = do num'      <- Gen.integral $ Range.linear 1 num
        pred      <- genExpForTypeEnv (FunT [] BoolT) (Map.union zE kE)
        (zE', ss') <- genStreams zE kE num'
        return (zE', SFilter pred ss')



genExpForValType :: C m => ValType -> m (Exp () Var Prim)
genExpForValType ty = shrink $ Gen.recursive Gen.choice
  [ genContextForType   ty
  , genPrimConstructor  ty ]
  -- When we can generate primitives, prefer them
  [ genPrimitiveForType ty
  , genPrimitiveForType ty
  , genLetForType ty ]

-- | Generate a well typed expression that has given type.
--
genExpForType :: C m => Type -> m (Exp () Var Prim)
genExpForType ty 
 = case ty of
    FunT (FunT [] t : ts) ret
      -> freshBind t (Priority 30) $ \n -> xLam n t <$> genExpForType (FunT ts ret)
    FunT (_:_) _
      -> Prelude.error "genExpForType: invalid higher order function type. We cannot generate these, so type generator should not include these."
    FunT [] ret
      -> genExpForValType ret
 where

genPrimConstructor :: C m => ValType -> m (Exp () Var Prim)
genPrimConstructor t
 = case t of
    IntT      -> genContextOrValue
    DoubleT   -> genContextOrValue
    UnitT     -> genContextOrValue
    ErrorT    -> genContextOrValue
    BoolT     -> genContextOrValue
    TimeT     -> genContextOrValue
    FactIdentifierT
              -> genContextOrValue
    StringT   -> genContextOrValue
    StructT _ -> genContextOrValue

    PairT a b -> valueChoice 
        [ (xPrim' $ PrimMinimal $ PM.PrimConst $ PM.PrimConstPair a b) `pApp` genExpForValType a `pApp` genExpForValType b ]

    SumT a b  -> valueChoice
        [ (xPrim' $ PrimMinimal $ PM.PrimConst $ PM.PrimConstLeft  a b) `pApp` genExpForValType a
        , (xPrim' $ PrimMinimal $ PM.PrimConst $ PM.PrimConstRight a b) `pApp` genExpForValType b ]

    OptionT a -> valueChoice
        [ (xPrim' $ PrimMinimal $ PM.PrimConst $ PM.PrimConstSome a) `pApp` genExpForValType a ]

    ArrayT a  -> valueChoice
        -- TODO: see BufT below
        -- [ (xPrim' $ PrimLatest  $ PrimLatestRead 1 a) `pApp` genExpForValType (BufT 1 a)
        [ (xPrim' $ PrimMinimal $ PM.PrimBuiltinFun $ PM.PrimBuiltinMap $ PM.PrimBuiltinVals IntT a) `pApp` genExpForValType (MapT IntT a)
        , (xPrim' $ PrimMinimal $ PM.PrimBuiltinFun $ PM.PrimBuiltinMap $ PM.PrimBuiltinKeys a IntT) `pApp` genExpForValType (MapT a IntT) ]

    BufT _ _ -> genContextOrValue
    -- TODO: fix buffers. Something wrong with the types of the primitives, to do with FactIdentifiers.
    -- Either fix the primitives or remove FactIdentifiers completely.
    --    genrec1
    --    (xValue t $ VBuf [])
    --    (\x' -> (xPrim' $ PrimLatest $ PrimLatestPush n a) `pApp` pure x' `pApp` genExpForValType FactIdentifierT `pApp` genExpForValType a)

    MapT k v -> genrec1
        (xValue t (VMap Map.empty) )
        (\x' -> (xPrim' $ PrimMap $ PrimMapInsertOrUpdate k v) `pApp` xid v `pApp` genExpForValType v `pApp` genExpForValType k `pApp` pure x' )
 where
  -- Make values very rare, but allow us to shrink to them
  valueChoice vs = Gen.frequency ((1, genValue) : fmap ((,) 10) vs)

  genValue = xValue t <$> baseValueForType t

  genrec1 nonrec rec =
   Gen.recursive Gen.choice
    [ pure nonrec, rec nonrec ]
    [ genContextForType t >>= rec ]

  xPrim' = pure . xPrim

  xid t' = genExpForType (FunT [FunT [] t'] t')

  pApp l r = do
    lx <- l
    rx <- r
    return (xApp lx rx)

  genContextOrValue = do
   c <- tryGenContextForType t
   case c of
    Nothing -> genValue
    Just c' -> return c'


-- | Generate an expression for an arbitrary value type
genExp :: C m => m (Exp () Var Prim, ValType)
genExp = do
  (env,prims) <- Reader.asks id
  let pts = Map.keys  prims
  let ets = fmap fst $ Map.elems env
  Gen.choice [ genXT ets <|> genXT pts
             , genXT pts ]
 where
  genXT ts
   | null ts   = Gen.discard
   | otherwise = do
    t <- Gen.element ts
    (,) <$> genExpForType (FunT [] t) <*> pure t


-- | Try to generate an expression with a given type from the context.
tryGenContextForType :: C m => ValType -> m (Maybe (Exp () Var Prim))
tryGenContextForType r = do
  env <- Reader.asks fst
  let m' = catMaybes $ fmap gen $ Map.toList env
  case List.null m' of
   True -> return Nothing
   False -> Just <$> Gen.frequency m'
 where
  gen (v,(t,Priority p)) = do
   x <- project (xVar v) t
   return (p, return x)

  project x t
   | t == r
   = Just x
   | PairT a b <- t
   =   project (prim (PrimMinimal $ PM.PrimPair $ PM.PrimPairFst a b) x) a
   <|> project (prim (PrimMinimal $ PM.PrimPair $ PM.PrimPairSnd a b) x) b
   | StructT st@(StructType fs) <- t
   = msum
    $ fmap (\(fn,ft) -> project (prim (PrimMinimal $ PM.PrimStruct $ PM.PrimStructGet fn ft st) x) ft)
    $ Map.toList fs
   | otherwise
   = Nothing

  prim p x = xPrim p `xApp` x

-- | Generate context lookup, falling back to constructors / values
genContextForType :: C m => ValType -> m (Exp () Var Prim)
genContextForType r = do
  e <- tryGenContextForType r
  case e of
   Nothing -> genPrimConstructor r
   Just e' -> return e'


-- | Try to generate a primitive of a given type
tryGenPrimitiveForType :: C m => ValType -> m (Maybe (Exp () Var Prim))
tryGenPrimitiveForType r = do
  primmap <- Reader.asks snd
  case Map.lookup r primmap of
   Nothing -> return Nothing
   Just ps -> do
    p <- Gen.element ps
    args <- mapM genExpForType (functionArguments $ typeOfPrim p)
    return $ Just $ foldl xApp (xPrim p) args

-- | Generate primitive application, falling back to context
genPrimitiveForType :: C m => ValType -> m (Exp () Var Prim)
genPrimitiveForType r = do
  p <- tryGenPrimitiveForType r
  case p of
   Nothing -> do
    genContextForType r
   Just p' -> return p'

-- | Generate a let binding with given return type.
genLetForType :: C m => ValType -> m (Exp () Var Prim)
genLetForType r = shrink $ do
  (x, t)   <- genExp
  freshBind t (Priority 50) $ \n -> xLet n x <$> genExpForType (FunT [] r)

-- Shrink the 
shrink :: C m => m (Exp () Var Prim) -> m (Exp () Var Prim)
shrink = Gen.shrink go
 where
  go x = runFixT (Simp.simpX ()) (Simp.deadX x)

  runFixT f a = case Identity.runIdentity $ Fix.runFixT $ f a of
   (a', Fix.RunAgain) -> [Identity.runIdentity $ Fix.fixpoint f a']
   _ -> []

