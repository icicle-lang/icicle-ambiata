{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Icicle.Sea.Eval.Base (
    MemPool
  , SeaState
  , SeaFleet(..)
  , SeaProgram(..)
  , SeaError(..)
  , CacheSea(..)
  , Input(..)
  , InputAllowDupTime(..)
  , PsvInputConfig(..)
  , PsvOutputConfig(..)
  , Mode(..)
  , PsvOutputFormat(..)
  , PsvInputFormat(..)
  , PsvInputDenseDict(..)

  , seaCompile
  , seaCompileFleet
  , seaCreate
  , seaCreateFleet
  , seaEval
  , seaRelease
  , seaEvalAvalanche

  , codeOfPrograms
  , assemblyOfPrograms
  , irOfPrograms
  , getCompilerOptions

  , withWords
  , pokeWordOff
  , peekWordOff
  ) where

import           Control.Monad.Catch (MonadMask(..))
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word (Word64)

import           Foreign.C.String (newCString, peekCString)
import           Foreign.ForeignPtr (ForeignPtr, touchForeignPtr, castForeignPtr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Foreign.Marshal (mallocBytes, free)
import           Foreign.Ptr (Ptr, WordPtr, ptrToWordPtr, wordPtrToPtr)
import           Foreign.Storable (Storable(..))

import           Icicle.Avalanche.Prim.Flat (Prim, tryMeltType)
import           Icicle.Avalanche.Prim.Eval (unmeltValue)
import           Icicle.Avalanche.Program (Program)

import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Data (asAtValueToCore, valueFromCore)
import           Icicle.Common.Type (ValType(..), StructType(..), defaultOfType)

import           Icicle.Data (Attribute(..))
import qualified Icicle.Data as D
import           Icicle.Data.Time (packedOfTime, timeOfPacked)

import           Icicle.Internal.Pretty (pretty, vsep)
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty)

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Program (seaOfProgram)
import           Icicle.Sea.FromAvalanche.State (stateOfProgram)
import           Icicle.Sea.FromAvalanche.Type (seaOfDefinitions)
import           Icicle.Sea.IO
import           Icicle.Sea.Preamble (seaPreamble)
import           Icicle.Sea.Fleet
import           Icicle.Sea.Eval.Program

import           Jetski

import           P hiding (count)

import           System.Environment (lookupEnv)
import           System.IO (IO, FilePath)
import           System.IO.Unsafe (unsafePerformIO)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT')
import           X.Control.Monad.Trans.Either (firstEitherT, hoistEither, left)


------------------------------------------------------------------------

data CacheSea =
    NoCacheSea
  | CacheSea
    deriving (Eq, Ord, Show)

data SeaMVector
  = I64 (IOVector Int64)
  | U64 (IOVector Word64)
  | F64 (IOVector Double)
  | P64 (IOVector WordPtr) ValType

instance Show SeaMVector where
  showsPrec p sv = showParen (p > 10) $ case sv of
    I64 v   -> showString "I64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    U64 v   -> showString "U64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    F64 v   -> showString "F64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    P64 v t -> showString "P64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v)) . showString " " . showsPrec 11 t

------------------------------------------------------------------------

seaEvalAvalanche
  :: (Show a, Show n, Pretty n, Eq n)
  => Program (Annot a) n Prim
  -> D.Time
  -> [D.AsAt D.Value]
  -> EitherT SeaError IO [(OutputName, D.Value)]
seaEvalAvalanche program time values = do
  let attr = Attribute "eval"
      ps   = Map.singleton attr program
  bracketEitherT'
    (seaCompile CacheSea NoInput ps Nothing)
    seaRelease
    (\fleet -> do
        programs <- mkSeaPrograms (sfLibrary fleet) ps
        seaEval attr programs fleet time values)

seaEval
  :: (MonadIO m, MonadMask m)
  => Attribute
  -> Map Attribute SeaProgram
  -> SeaFleet st
  -> D.Time
  -> [D.AsAt D.Value]
  -> EitherT SeaError m [(OutputName, D.Value)]
seaEval attribute programs fleet time values =
  case Map.lookup attribute programs of
    Nothing      -> left (SeaProgramNotFound attribute)
    Just program -> do
      let create  = liftIO $ sfCreatePool  fleet
          release = liftIO . sfReleasePool fleet
      seaEval' program create release time values

seaEval'
  :: (MonadIO m, MonadMask m)
  => SeaProgram
  -> (EitherT SeaError m (Ptr MemPool))
  -> (Ptr MemPool -> EitherT SeaError m ())
  -> D.Time
  -> [D.AsAt D.Value]
  -> EitherT SeaError m [(OutputName, D.Value)]
seaEval' program createPool releasePool time values = do
  let words              = spStateWords program
      acquireFacts       = vectorsOfFacts values (spFactType program)
      releaseFacts facts = traverse_ freeSeaVector facts

  bracketEitherT' acquireFacts releaseFacts $ \facts -> do
  withWords      words $ \pState -> do
  withSeaVectors facts $ \count psFacts -> do

    let mempoolIx  = 0 :: Int
        dateIx     = 1
        countIx    = 2
        factsIx    = 3
        outputsIx  = 3 + length psFacts

    pokeWordOff pState dateIx  (packedOfTime time)
    pokeWordOff pState countIx (fromIntegral count :: Int64)

    zipWithM_ (pokeWordOff pState) [factsIx..] psFacts

    bracketEitherT' createPool releasePool $ \poolPtr -> do
      pokeWordOff pState mempoolIx poolPtr
      _       <- liftIO (spCompute program pState)
      outputs <- peekNamedOutputs pState outputsIx (spOutputs program)
      hoistEither (traverse (\(k,v) -> (,) <$> pure k <*> valueFromCore' v) outputs)

valueFromCore' :: BaseValue -> Either SeaError D.Value
valueFromCore' v =
  maybe (Left (SeaBaseValueConversionError v Nothing)) Right (valueFromCore v)

------------------------------------------------------------------------

seaCompile ::
     (MonadIO m)
  => (Show a, Show n, Pretty n, Eq n)
  => CacheSea
  -> Input
  -> Map Attribute (Program (Annot a) n Prim)
  -> Maybe FilePath
  -> EitherT SeaError m (SeaFleet st)
seaCompile cache input programs chords = do
  options <- getCompilerOptions
  seaCompileFleet options cache input programs chords

seaCompileFleet ::
     (MonadIO m)
  => (Show a, Show n, Pretty n, Eq n)
  => [CompilerOption]
  -> CacheSea
  -> Input
  -> Map Attribute (Program (Annot a) n Prim)
  -> Maybe FilePath
  -> EitherT SeaError m (SeaFleet st)
seaCompileFleet options cache input programs chords = do
  code <- hoistEither (codeOfPrograms input (Map.toList programs))
  seaCreateFleet options (fromCacheSea cache) input chords code

seaCreate ::
     (MonadIO m)
  => CacheSea
  -> Input
  -> Maybe FilePath
  -> Text
  -> EitherT SeaError m (SeaFleet st)
seaCreate cache input chords code = do
  options <- getCompilerOptions
  seaCreateFleet options (fromCacheSea cache) input chords code

fromCacheSea :: CacheSea -> CacheLibrary
fromCacheSea = \case
  NoCacheSea ->
    NoCacheLibrary
  CacheSea ->
    CacheLibrary

seaRelease :: MonadIO m => SeaFleet st -> m ()
seaRelease fleet =
  releaseLibrary (sfLibrary fleet)

getCompilerOptions :: MonadIO m => m [CompilerOption]
getCompilerOptions = do
  menv <- liftIO $ lookupEnv "ICICLE_CC_OPTIONS"
  case menv of
    Nothing  -> return $ defaultCompilerOptions
    Just env -> return $ defaultCompilerOptions <> T.splitOn " " (T.pack env)

defaultCompilerOptions :: [CompilerOption]
defaultCompilerOptions =
  [ "-O3"           -- 🔨
  , "-g3"           -- include debug information (3 = as much as possible)
  , "-march=native" -- all optimisations valid for the current CPU (AVX512, etc)
  , "-std=c99"      -- variable declarations anywhere!
  ]

assemblyOfPrograms
  :: (Show a, Show n, Pretty n, Eq n)
  => Input
  -> [(Attribute, Program (Annot a) n Prim)]
  -> EitherT SeaError IO Text
assemblyOfPrograms input programs = do
  code    <- hoistEither (codeOfPrograms input programs)
  options <- getCompilerOptions
  firstEitherT SeaJetskiError (compileAssembly options code)

irOfPrograms
  :: (Show a, Show n, Pretty n, Eq n)
  => Input
  -> [(Attribute, Program (Annot a) n Prim)]
  -> EitherT SeaError IO Text
irOfPrograms input programs = do
  code    <- hoistEither (codeOfPrograms input programs)
  options <- getCompilerOptions
  firstEitherT SeaJetskiError (compileIR options code)

codeOfPrograms
  :: (Show a, Show n, Pretty n, Eq n)
  => Input
  -> [(Attribute, Program (Annot a) n Prim)]
  -> Either SeaError Text
codeOfPrograms input programs = do
  let defs = seaOfDefinitions (fmap snd programs)

  progs   <- zipWithM (\ix (a, p) -> seaOfProgram   ix a p) [0..] programs
  states  <- zipWithM (\ix (a, p) -> stateOfProgram ix a p) [0..] programs

  let defOfPsvInput conf
        | inputPsvFormat conf == PsvInputSparse
        = "#define ICICLE_PSV_INPUT_SPARSE 1"
        | otherwise
        = ""

  let defOfPsvOutput conf
        | outputPsvFormat conf == PsvOutputSparse
        = "#define ICICLE_PSV_OUTPUT_SPARSE 1"
        | otherwise
        = ""

  case input of
    NoInput -> do
      let consts = seaOfDefaultConstants
      pure . textOfDoc . vsep
        $ [ "#define ICICLE_NO_INPUT 1"
          , consts
          , seaPreamble
          , defs
          ] <> progs
    HasInput format opts _ -> do
      doc <- seaOfDriver format opts states
      let cnst = case format of
                   FormatPsv conf
                     -> seaOfConstants conf
                   _ -> seaOfDefaultConstants
      let def  = case format of
                   FormatPsv conf
                     -> vsep [ defOfPsvInput (psvInputConfig conf), defOfPsvOutput (psvOutputConfig conf) ]
                   _ -> vsep [ -- FIXME property separate what is needed from psv
                               "#define ICICLE_PSV_INPUT_SPARSE 1"
                             , "#define ICICLE_ZEBRA 1"
                             ]

      pure . textOfDoc . vsep $ [def, cnst, seaPreamble, defs] <> progs <> ["", doc]

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")

------------------------------------------------------------------------

lengthOfSeaVector :: SeaMVector -> Int
lengthOfSeaVector = \case
  I64 v   -> MV.length v
  U64 v   -> MV.length v
  F64 v   -> MV.length v
  P64 v _ -> MV.length v

ptrOfSeaVector :: SeaMVector -> ForeignPtr Word64
ptrOfSeaVector = \case
  I64 v   -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  U64 v   -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  F64 v   -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  P64 v _ -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v

freeSeaVector :: MonadIO m => SeaMVector -> m ()
freeSeaVector = \case
  I64 _  -> return ()
  U64 _  -> return ()
  F64 _  -> return ()
  P64 mv t -> do
    v <- liftIO (V.unsafeFreeze mv)
    liftIO $ V.mapM_ (free' t) v
 where
  free' (ArrayT t) x
   | StringT <- t
   = do let ptr = wordPtrToPtr x
        sz <- peekWordOff ptr 0
        mapM_ (\i -> peekWordOff ptr i >>= free) [1..sz]
        freeWordPtr x
  free' _ x
   = freeWordPtr x


withSeaVectors :: MonadIO m
               => [SeaMVector]
               -> (Int -> [Ptr Word64] -> EitherT SeaError m a)
               -> EitherT SeaError m a
withSeaVectors []       io = io 0 []
withSeaVectors (sv:svs) io =
  withSeaVector  sv  $ \len ptr  ->
  withSeaVectors svs $ \_   ptrs ->
  io len (ptr : ptrs)

withSeaVector :: MonadIO m
              => SeaMVector
              -> (Int -> Ptr Word64 -> EitherT SeaError m a)
              -> EitherT SeaError m a
withSeaVector sv io =
  withForeignPtr (ptrOfSeaVector sv) (io (lengthOfSeaVector sv))

------------------------------------------------------------------------

vectorsOfFacts :: MonadIO m => [D.AsAt D.Value] -> ValType -> EitherT SeaError m [SeaMVector]
vectorsOfFacts vs t = do
  case traverse (\v -> asAtValueToCore v t) vs of
    Nothing  -> left (SeaFactConversionError vs t)
    Just vs' -> do
      svs <- newSeaVectors (length vs') t
      zipWithM_ (pokeInput svs t) [0..] vs'
      pure svs

newSeaVectors :: MonadIO m => Int -> ValType -> EitherT SeaError m [SeaMVector]
newSeaVectors sz t =
  case t of
    IntT    -> (:[]) . I64 <$> liftIO (MV.new sz)
    DoubleT -> (:[]) . F64 <$> liftIO (MV.new sz)
    UnitT   -> (:[]) . U64 <$> liftIO (MV.new sz)
    BoolT   -> (:[]) . U64 <$> liftIO (MV.new sz)
    TimeT   -> (:[]) . U64 <$> liftIO (MV.new sz)
    ErrorT  -> (:[]) . U64 <$> liftIO (MV.new sz)
    FactIdentifierT
            -> (:[]) . I64 <$> liftIO (MV.new sz)
    StringT -> (:[]) . flip P64 t <$> liftIO (MV.new sz)

    BufT{}    -> left (SeaTypeConversionError t)

    ArrayT _
     -> (:[]) . flip P64 t <$> liftIO (MV.new sz)

    MapT tk tv
     -> do vk <- newSeaVectors sz (ArrayT tk)
           vv <- newSeaVectors sz (ArrayT tv)
           pure (vk <> vv)

    PairT ta tb
     -> do va <- newSeaVectors sz ta
           vb <- newSeaVectors sz tb
           pure (va <> vb)

    SumT ErrorT tb
     -> do va <- newSeaVectors sz ErrorT
           vb <- newSeaVectors sz tb
           pure (va <> vb)

    SumT ta tb
     -> do vi <- newSeaVectors sz BoolT
           va <- newSeaVectors sz ta
           vb <- newSeaVectors sz tb
           pure (vi <> va <> vb)

    OptionT tx
     -> do vb <- newSeaVectors sz BoolT
           vx <- newSeaVectors sz tx
           pure (vb <> vx)

    StructT (StructType ts)
     -> do vss <- traverse (newSeaVectors sz) (Map.elems ts)
           pure (concat vss)

pokeInput :: MonadIO m => [SeaMVector] -> ValType -> Int -> BaseValue -> EitherT SeaError m ()
pokeInput svs t ix val = do
  svs' <- pokeInput' svs t ix val
  case svs' of
    [] -> pure ()
    _  -> left (SeaBaseValueConversionError val (Just t))

pokeInput' :: MonadIO m => [SeaMVector] -> ValType -> Int -> BaseValue -> EitherT SeaError m [SeaMVector]
pokeInput' []            t _  val = left (SeaBaseValueConversionError val (Just t))
pokeInput' svs0@(sv:svs) t ix val =
  case (sv, val, t) of
    (U64 v, VBool False, BoolT)   -> pure svs <* liftIO (MV.write v ix 0)
    (U64 v, VBool  True, BoolT)   -> pure svs <* liftIO (MV.write v ix 1)
    (I64 v, VInt      x, IntT)    -> pure svs <* liftIO (MV.write v ix (fromIntegral x))
    (F64 v, VDouble   x, DoubleT) -> pure svs <* liftIO (MV.write v ix x)
    (U64 v, VTime     x, TimeT)   -> pure svs <* liftIO (MV.write v ix (packedOfTime x))
    (U64 v, VError    x, ErrorT)  -> pure svs <* liftIO (MV.write v ix (wordOfError x))

    (I64 v, VFactIdentifier x, FactIdentifierT)
                                  -> pure svs <* liftIO (MV.write v ix $ fromIntegral $ getFactIdentifierIndex x)

    (P64 v _, VString xs, StringT)
     -> do let str = T.unpack xs
           ptr <- ptrToWordPtr <$> liftIO (newCString str)
           liftIO (MV.write v ix ptr)
           pure svs

    (P64 v _, VArray xs, ArrayT tx)
     -> do ptr :: Ptr Word64 <- liftIO (mallocWords (length xs + 1))
           pokeArray ptr tx xs
           liftIO (MV.write v ix (ptrToWordPtr ptr))
           pure svs

    (_, VMap kvs, MapT tk tv)
     -> do svs1 <- pokeInput' svs0 (ArrayT tk) ix (VArray (Map.keys  kvs))
           svs2 <- pokeInput' svs1 (ArrayT tv) ix (VArray (Map.elems kvs))
           pure svs2

    (_, VPair a b, PairT ta tb)
     -> do svs1 <- pokeInput' svs0 ta ix a
           svs2 <- pokeInput' svs1 tb ix b
           pure svs2

    (_, VNone, OptionT tx)
     -> do svs1 <- pokeInput' svs0 BoolT ix (VBool False)
           svs2 <- pokeInput' svs1 tx    ix (defaultOfType tx)
           pure svs2

    (_, VSome x, OptionT tx)
     -> do svs1 <- pokeInput' svs0 BoolT ix (VBool True)
           svs2 <- pokeInput' svs1 tx    ix x
           pure svs2

    (_, VLeft a, SumT ta tb)
     | ErrorT <- ta
     -> do svs1 <- pokeInput' svs0 ta    ix a
           svs2 <- pokeInput' svs1 tb    ix (defaultOfType tb)
           pure svs2

    (_, VLeft a, SumT ta tb)
     -> do svs1 <- pokeInput' svs0 BoolT ix (VBool False)
           svs2 <- pokeInput' svs1 ta    ix a
           svs3 <- pokeInput' svs2 tb    ix (defaultOfType tb)
           pure svs3

    (_, VRight b, SumT ta tb)
     | ErrorT <- ta
     -> do svs1 <- pokeInput' svs0 ta    ix (VError ExceptNotAnError)
           svs2 <- pokeInput' svs1 tb    ix b
           pure svs2

    (_, VRight b, SumT ta tb)
     -> do svs1 <- pokeInput' svs0 BoolT ix (VBool True)
           svs2 <- pokeInput' svs1 ta    ix (defaultOfType ta)
           svs3 <- pokeInput' svs2 tb    ix b
           pure svs3

    (_, VStruct xs, StructT (StructType ts))
     -> do let pokeField svs1 (f, tf) =
                 case Map.lookup f xs of
                   Nothing -> left (SeaBaseValueConversionError val (Just t))
                   Just vf -> pokeInput' svs1 tf ix vf

           foldM pokeField svs0 (Map.toList ts)

    _
     -> left (SeaBaseValueConversionError val (Just t))

------------------------------------------------------------------------

peekNamedOutputs
  :: MonadIO m
  => Ptr a
  -> Int
  -> [(OutputName, (ValType, [ValType]))]
  -> EitherT SeaError m [(OutputName, BaseValue)]

peekNamedOutputs _ _ []                     = pure []
peekNamedOutputs ptr ix ((n, (t, _)) : ots) = do
  nvs    <- peekNamedOutputs ptr (ix+1) ots
  (_, v) <- peekOutput  ptr ix t
  pure ((n, v) : nvs)


peekOutputs
  :: MonadIO m
  => Ptr a
  -> Int
  -> [ValType]
  -> EitherT SeaError m (Int, [BaseValue])

peekOutputs _   ix0 []       = pure (ix0, [])
peekOutputs ptr ix0 (t : ts) = do
  (ix1, v)  <- peekOutput  ptr ix0 t
  (ix2, vs) <- peekOutputs ptr ix1 ts
  pure (ix2, v : vs)


peekOutput :: MonadIO m => Ptr a -> Int -> ValType -> EitherT SeaError m (Int, BaseValue)
peekOutput ptr ix0 t =
  case t of
    UnitT   -> (ix0+1,)                          <$> pure VUnit
    IntT    -> (ix0+1,) . VInt    . fromInt64    <$> peekWordOff ptr ix0
    DoubleT -> (ix0+1,) . VDouble                <$> peekWordOff ptr ix0
    TimeT   -> (ix0+1,) . VTime   . timeOfPacked <$> peekWordOff ptr ix0
    ErrorT  -> (ix0+1,) . VError  . errorOfWord  <$> peekWordOff ptr ix0
    FactIdentifierT
            -> (ix0+1,) . VFactIdentifier . FactIdentifier . fromInt64 <$> peekWordOff ptr ix0

    StructT _
     | Just ts <- tryMeltType t
     -> do (ix1, oss) <- peekOutputs ptr ix0 ts
           v <- unmeltValueE (SeaTypeConversionError t) oss t
           pure (ix1, v)
     | otherwise -> left (SeaTypeConversionError t)

    BufT{}    -> left (SeaTypeConversionError t)

    StringT
     -> do strPtr <- wordPtrToPtr <$> peekWordOff ptr ix0
           str    <- liftIO (peekCString strPtr)
           pure (ix0+1, VString (T.pack str))

    ArrayT tx
     | Just ts <- tryMeltType t
     -> do (ix1, oss) <- peekOutputs ptr ix0 ts
           v <- unmeltValueE (SeaTypeConversionError t) oss t
           pure (ix1, v)

     | otherwise
     -> do arrPtr :: Ptr Word64 <- wordPtrToPtr <$> peekWordOff ptr ix0
           xs <- peekArray arrPtr tx
           pure (ix0+1, VArray xs)

    BoolT
     -> do b <- peekWordOff ptr ix0
           case b :: Word64 of
             0 -> pure (ix0+1, VBool False)
             _ -> pure (ix0+1, VBool True)

    MapT tk tv
     -> do (ix1, vk) <- peekOutput ptr ix0 (ArrayT tk)
           (ix2, vv) <- peekOutput ptr ix1 (ArrayT tv)
           ak <- unArray (SeaTypeConversionError t) vk
           av <- unArray (SeaTypeConversionError t) vv
           pure (ix2, VMap (Map.fromList (List.zip ak av)))

    PairT ta tb
     -> do (ix1, va) <- peekOutput ptr ix0 ta
           (ix2, vb) <- peekOutput ptr ix1 tb
           pure (ix2, VPair va vb)

    SumT ErrorT tb
     -> do (ix1, vi) <- peekOutput ptr ix0 ErrorT
           (ix2, vb) <- peekOutput ptr ix1 tb
           pure (ix2, if vi == VError ExceptNotAnError then VRight vb else VLeft vi)

    SumT ta tb
     -> do (ix1, vi) <- peekOutput ptr ix0 BoolT
           (ix2, va) <- peekOutput ptr ix1 ta
           (ix3, vb) <- peekOutput ptr ix2 tb
           pure (ix3, if vi == VBool False then VLeft va else VRight vb)

    OptionT tx
     -> do (ix1, vb) <- peekOutput ptr ix0 BoolT
           (ix2, vx) <- peekOutput ptr ix1 tx
           pure (ix2, if vb == VBool False then VNone else VSome vx)

------------------------------------------------------------------------

pokeArray :: MonadIO m => Ptr x -> ValType -> [BaseValue] -> EitherT SeaError m ()
pokeArray ptr t vs = do
  let len = fromIntegral (length vs) :: Int64
  liftIO (pokeWordOff ptr 0 len)
  zipWithM_ (pokeArrayIx ptr t) [1..] vs

pokeArrayIx :: MonadIO m => Ptr x -> ValType -> Int -> BaseValue -> EitherT SeaError m ()
pokeArrayIx ptr t ix v =
  case (v, t) of
    (VBool False, BoolT)   -> liftIO (pokeWordOff ptr ix (0 :: Word64))
    (VBool  True, BoolT)   -> liftIO (pokeWordOff ptr ix (1 :: Word64))
    (VInt      x, IntT)    -> liftIO (pokeWordOff ptr ix (fromIntegral x :: Int64))
    (VDouble   x, DoubleT) -> liftIO (pokeWordOff ptr ix x)
    (VTime     x, TimeT)   -> liftIO (pokeWordOff ptr ix (packedOfTime x))
    (VFactIdentifier x, FactIdentifierT)
                           -> liftIO (pokeWordOff ptr ix (fromIntegral (getFactIdentifierIndex x) :: Int64))
    (VError    x, ErrorT)  -> liftIO (pokeWordOff ptr ix (wordOfError x))
    (VString   x, StringT) -> liftIO (newCString (T.unpack x) >>= pokeWordOff ptr ix)
    _                      -> left (SeaBaseValueConversionError v (Just t))

peekArray :: MonadIO m => Ptr x -> ValType -> EitherT SeaError m [BaseValue]
peekArray ptr t = do
  len <- peekWordOff ptr 0
  traverse (peekArrayIx ptr t) [1..len]

peekArrayIx :: MonadIO m => Ptr x -> ValType -> Int -> EitherT SeaError m BaseValue
peekArrayIx ptr t ix =
  case t of
    UnitT   -> pure VUnit
    IntT    -> VInt    . fromInt64    <$> peekWordOff ptr ix
    DoubleT -> VDouble                <$> peekWordOff ptr ix
    TimeT   -> VTime   . timeOfPacked <$> peekWordOff ptr ix
    FactIdentifierT
            -> VFactIdentifier . FactIdentifier . fromInt64 <$> peekWordOff ptr ix
    ErrorT  -> VError  . errorOfWord  <$> peekWordOff ptr ix

    BoolT
     -> do b <- peekWordOff ptr ix
           case b :: Word64 of
             0 -> pure (VBool False)
             _ -> pure (VBool True)

    StringT
     -> do strPtr <- wordPtrToPtr <$> peekWordOff ptr ix
           str    <- liftIO (peekCString strPtr)
           pure (VString (T.pack str))

    ArrayT tx
     -> do a  <- peekWordOff ptr ix
           as <- peekArray   a   tx
           pure $ VArray as

    _
     -> left (SeaTypeConversionError (ArrayT t))

unArray :: Monad m => e -> BaseValue -> EitherT e m [BaseValue]
unArray _ (VArray vs) = pure vs
unArray e _           = left e

unmeltValueE :: Monad m => e -> [BaseValue] -> ValType -> EitherT e m BaseValue
unmeltValueE e vs t = maybe (left e) pure (unmeltValue vs t)

------------------------------------------------------------------------

withForeignPtr :: MonadIO m => ForeignPtr a -> (Ptr a -> EitherT SeaError m b) -> EitherT SeaError m b
withForeignPtr fp io = do
  x <- io (unsafeForeignPtrToPtr fp)
  liftIO (touchForeignPtr fp)
  pure x

withWords :: (MonadIO m, MonadMask m) => Int -> (Ptr a -> EitherT SeaError m b) -> EitherT SeaError m b
withWords n io =
  bracketEitherT' (mallocWords n) (liftIO . free) $ \ptr -> do
    forM_ [0..(n-1)] $ \off ->
      pokeWordOff ptr off (0 :: Word64)
    io ptr

mallocWords :: MonadIO m => Int -> m (Ptr a)
mallocWords n = liftIO (mallocBytes (n*8))

freeWordPtr :: MonadIO m => WordPtr -> m ()
freeWordPtr wp = do
  let ptr :: Ptr Word64 = wordPtrToPtr wp
  liftIO (free ptr)

pokeWordOff :: (MonadIO m, Storable a) => Ptr x -> Int -> a -> m ()
pokeWordOff ptr off x = liftIO (pokeByteOff ptr (off*8) x)

peekWordOff :: (MonadIO m, Storable a) => Ptr x -> Int -> m a
peekWordOff ptr off = liftIO (peekByteOff ptr (off*8))

fromInt64 :: Int64 -> Int
fromInt64 = fromIntegral

wordOfError :: ExceptionInfo -> Word64
wordOfError = \case
  ExceptNotAnError                 -> 0
  ExceptTombstone                  -> 1
  ExceptFold1NoValue               -> 2
  ExceptScalarVariableNotAvailable -> 3

errorOfWord :: Word64 -> ExceptionInfo
errorOfWord = \case
  0 -> ExceptNotAnError
  1 -> ExceptTombstone
  2 -> ExceptFold1NoValue
  3 -> ExceptScalarVariableNotAvailable
  _ -> ExceptTombstone
