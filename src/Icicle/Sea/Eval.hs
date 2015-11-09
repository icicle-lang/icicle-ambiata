{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Sea.Eval (
    MemPool
  , PsvState
  , SeaState
  , SeaFleet (..)
  , SeaProgram (..)
  , SeaError (..)

  , seaCompile
  , seaEval
  , seaPsvSnapshot
  , seaRelease

  , seaEvalAvalanche

  , assemblyOfPrograms
  , compilerOptions
  ) where

import           Control.Monad.Catch (MonadMask(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Either (EitherT(..), hoistEither, left)

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word (Word64)

import           Foreign.C.String (CString, newCString, peekCString)
import           Foreign.ForeignPtr (ForeignPtr, touchForeignPtr, castForeignPtr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           Foreign.Marshal (mallocBytes, free)
import           Foreign.Ptr (Ptr, WordPtr, ptrToWordPtr, wordPtrToPtr, castPtr, nullPtr)
import           Foreign.Storable (Storable(..))

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.Statement.Statement (FactLoopType(..))
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Data (asAtValueToCore, valueFromCore)
import           Icicle.Common.Type (ValType(..), StructType(..), defaultOfType)
import           Icicle.Data (Attribute(..))
import qualified Icicle.Data as D
import           Icicle.Data.DateTime (packedOfDate, dateOfPacked)
import           Icicle.Internal.Pretty ((<+>), pretty, text, vsep)
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty)
import           Icicle.Sea.FromAvalanche.Analysis (factVarsOfProgram, outputsOfProgram)
import           Icicle.Sea.FromAvalanche.Program (seaOfProgram, programName, stateWordsOfProgram)
import           Icicle.Sea.Preamble (seaPreamble)

import           Jetski

import           P hiding (count)

import           System.IO (IO, FilePath)
import           System.IO.Unsafe (unsafePerformIO)

import           X.Control.Monad.Catch (bracketEitherT')
import           X.Control.Monad.Trans.Either (firstEitherT)

------------------------------------------------------------------------

data MemPool
data PsvState
data SeaState

data SeaFleet = SeaFleet {
    sfLibrary     :: Library
  , sfPrograms    :: Map Attribute SeaProgram
  , sfCreatePool  :: Ptr MemPool  -> IO ()
  , sfReleasePool :: Ptr MemPool  -> IO ()
  , sfPsvSnapshot :: Ptr PsvState -> IO ()
  }

data SeaProgram = SeaProgram {
    spName        :: Int
  , spStateWords  :: Int
  , spFactType    :: ValType
  , spOutputs     :: [(OutputName, (ValType, [ValType]))]
  , spCompute     :: Ptr SeaState -> IO ()
  }

data SeaMVector
  = I64 (IOVector Int64)
  | U64 (IOVector Word64)
  | F64 (IOVector Double)
  | P64 (IOVector WordPtr)

instance Show SeaMVector where
  showsPrec p sv = showParen (p > 10) $ case sv of
    I64 v -> showString "I64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    U64 v -> showString "U64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    F64 v -> showString "F64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))
    P64 v -> showString "P64 " . showsPrec 11 (unsafePerformIO (V.unsafeFreeze v))

data SeaError
  = SeaJetskiError              JetskiError
  | SeaPsvError                 Text
  | SeaProgramNotFound          Attribute
  | SeaFactConversionError      [D.AsAt D.Value] ValType
  | SeaBaseValueConversionError BaseValue (Maybe ValType)
  | SeaTypeConversionError      ValType
  | SeaNoFactLoop
  | SeaNoOutputs
  deriving (Eq, Show)

instance Pretty SeaError where
  pretty = \case
    SeaFactConversionError vs t
     -> text "Cannot convert facts "
     <> pretty (fmap D.fact vs) <+> text ": [" <> pretty t <> text "]"

    SeaBaseValueConversionError v Nothing
     -> text "Cannot convert value " <> pretty v

    SeaBaseValueConversionError v (Just t)
     -> text "Cannot convert value " <> pretty v <+> text ":" <+> pretty t

    SeaTypeConversionError t
     -> text "Cannot convert type " <> pretty t

    SeaNoFactLoop
     -> text "No fact loop"

    SeaNoOutputs
     -> text "No outputs"

    SeaProgramNotFound attr
     -> text "Program for attribute \"" <> pretty attr <> "\" not found"

    SeaJetskiError (CompilerError _ _ stderr)
     -> pretty stderr

    SeaJetskiError je
     -> pretty (show je)

    SeaPsvError pe
     -> pretty pe

------------------------------------------------------------------------

seaPsvSnapshot :: SeaFleet -> FilePath -> FilePath -> EitherT SeaError IO ()
seaPsvSnapshot fleet input output =
  withWords   3      $ \pState  ->
  withCString input  $ \pInput  ->
  withCString output $ \pOutput -> do

  pokeWordOff pState 0 pInput
  pokeWordOff pState 1 pOutput

  liftIO (sfPsvSnapshot fleet pState)

  pError <- peekWordOff pState 2

  when (pError /= nullPtr) $ do
    msg <- liftIO (peekCString pError)
    left (SeaPsvError (T.pack msg))

  return ()

------------------------------------------------------------------------

seaEvalAvalanche
  :: (Show a, Show n, Pretty n, Ord n)
  => Program (Annot a) n Prim
  -> D.DateTime
  -> [D.AsAt D.Value]
  -> EitherT SeaError IO [(OutputName, D.Value)]
seaEvalAvalanche program date values = do
  let attr = Attribute "eval"
      ps   = Map.singleton attr program
  bracketEitherT' (seaCompile ps) seaRelease (\fleet -> seaEval attr fleet date values)

seaEval
  :: (MonadIO m, MonadMask m)
  => Attribute
  -> SeaFleet
  -> D.DateTime
  -> [D.AsAt D.Value]
  -> EitherT SeaError m [(OutputName, D.Value)]
seaEval attribute fleet date values =
  case Map.lookup attribute (sfPrograms fleet) of
    Nothing      -> left (SeaProgramNotFound attribute)
    Just program -> do
      let create  = liftIO . sfCreatePool  fleet
          release = liftIO . sfReleasePool fleet
      seaEval' program create release date values

seaEval'
  :: (MonadIO m, MonadMask m)
  => SeaProgram
  -> (Ptr MemPool -> EitherT SeaError m ())
  -> (Ptr MemPool -> EitherT SeaError m ())
  -> D.DateTime
  -> [D.AsAt D.Value]
  -> EitherT SeaError m [(OutputName, D.Value)]
seaEval' program createPool releasePool date values = do
  let words              = spStateWords program
      acquireFacts       = vectorsOfFacts values (spFactType program)
      releaseFacts facts = traverse_ freeSeaVector facts

  bracketEitherT' acquireFacts releaseFacts $ \facts -> do
  withWords      words $ \pState -> do
  withSeaVectors facts $ \count psFacts -> do

    let _mempoolIx = 0 :: Int
        dateIx     = 3
        countIx    = 4
        factsIx    = 5
        outputsIx  = 5 + length psFacts

    pokeWordOff pState dateIx  (packedOfDate date)
    pokeWordOff pState countIx (fromIntegral count :: Int64)

    zipWithM_ (pokeWordOff pState) [factsIx..] psFacts

    let pMemPool = castPtr pState

    bracketEitherT' (createPool pMemPool) (const $ releasePool pMemPool) $ \_ -> do
      _       <- liftIO (spCompute program pState)
      outputs <- peekOutputs pState outputsIx (spOutputs program)
      hoistEither (traverse (\(k,v) -> (,) <$> pure k <*> valueFromCore' v) outputs)

valueFromCore' :: BaseValue -> Either SeaError D.Value
valueFromCore' v =
  maybe (Left (SeaBaseValueConversionError v Nothing)) Right (valueFromCore v)

------------------------------------------------------------------------

seaCompile
  :: (MonadIO m, MonadMask m, Functor m)
  => (Show a, Show n, Pretty n, Ord n)
  => Map Attribute (Program (Annot a) n Prim)
  -> EitherT SeaError m SeaFleet
seaCompile programs = do
  let code = codeOfPrograms . fmap (first D.getAttribute) $ Map.toList programs

  lib             <- firstEitherT SeaJetskiError (compileLibrary compilerOptions code)
  imempool_create <- firstEitherT SeaJetskiError (function lib "imempool_create" retVoid)
  imempool_free   <- firstEitherT SeaJetskiError (function lib "imempool_free"   retVoid)
  psv_snapshot    <- firstEitherT SeaJetskiError (function lib "psv_snapshot"    retVoid)

  compiled <- zipWithM (mkSeaProgram lib) [0..] (Map.elems programs)

  return SeaFleet {
      sfLibrary     = lib
    , sfPrograms    = Map.fromList (List.zip (Map.keys programs) compiled)
    , sfCreatePool  = \ptr -> imempool_create [argPtr ptr]
    , sfReleasePool = \ptr -> imempool_free   [argPtr ptr]
    , sfPsvSnapshot = \ptr -> psv_snapshot    [argPtr ptr]
    }

mkSeaProgram
  :: (MonadIO m, MonadMask m, Functor m, Ord n)
  => Library
  -> Int
  -> Program (Annot a) n Prim
  -> EitherT SeaError m SeaProgram
mkSeaProgram lib name program = do
  let words   = stateWordsOfProgram program
      outputs = outputsOfProgram program

  factType <- case factVarsOfProgram FactLoopNew program of
                Nothing     -> left SeaNoFactLoop
                Just (t, _) -> return t

  compute <- firstEitherT SeaJetskiError (function lib (programName name) retVoid)

  return SeaProgram {
      spName       = name
    , spStateWords = words
    , spFactType   = factType
    , spOutputs    = outputs
    , spCompute    = \ptr -> compute [argPtr ptr]
    }

seaRelease :: MonadIO m => SeaFleet -> m ()
seaRelease fleet =
  releaseLibrary (sfLibrary fleet)

compilerOptions :: [CompilerOption]
compilerOptions =
  [ "-O3"           -- 🔨
  , "-march=native" -- 🚀  all optimisations valid for the current CPU (AVX512, etc)
  , "-std=c99"      -- 👹  variable declarations anywhere!
  , "-fPIC"         -- 🌏  position independent code, required on Linux
  ]

assemblyOfPrograms
  :: (Show a, Show n, Pretty n, Ord n)
  => [(Text, Program (Annot a) n Prim)]
  -> EitherT SeaError IO Text
assemblyOfPrograms programs = do
  let code = codeOfPrograms programs
  firstEitherT SeaJetskiError (compileAssembly compilerOptions code)

codeOfPrograms
  :: (Show a, Show n, Pretty n, Ord n)
  => [(Text, Program (Annot a) n Prim)]
  -> Text
codeOfPrograms programs = do
  let docs = List.zipWith (\ix (info,p) -> seaOfProgram ix info p) [0..] programs
  textOfDoc . vsep $ [seaPreamble] <> docs

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")

------------------------------------------------------------------------

lengthOfSeaVector :: SeaMVector -> Int
lengthOfSeaVector = \case
  I64 v -> MV.length v
  U64 v -> MV.length v
  F64 v -> MV.length v
  P64 v -> MV.length v

ptrOfSeaVector :: SeaMVector -> ForeignPtr Word64
ptrOfSeaVector = \case
  I64 v -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  U64 v -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  F64 v -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v
  P64 v -> castForeignPtr . fst $ MV.unsafeToForeignPtr0 v

freeSeaVector :: MonadIO m => SeaMVector -> m ()
freeSeaVector = \case
  I64 _  -> return ()
  U64 _  -> return ()
  F64 _  -> return ()
  P64 mv -> do
    v <- liftIO (V.unsafeFreeze mv)
    V.mapM_ freeWordPtr v


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
    IntT      -> (:[]) . I64 <$> liftIO (MV.new sz)
    DoubleT   -> (:[]) . F64 <$> liftIO (MV.new sz)
    UnitT     -> (:[]) . U64 <$> liftIO (MV.new sz)
    BoolT     -> (:[]) . U64 <$> liftIO (MV.new sz)
    DateTimeT -> (:[]) . U64 <$> liftIO (MV.new sz)
    ErrorT    -> (:[]) . U64 <$> liftIO (MV.new sz)
    StringT   -> (:[]) . P64 <$> liftIO (MV.new sz)

    MapT{}    -> left (SeaTypeConversionError t)
    BufT{}    -> left (SeaTypeConversionError t)

    ArrayT tx
     | StringT <- tx
     -> left (SeaTypeConversionError t)

     | otherwise
     -> (:[]) . P64 <$> liftIO (MV.new sz)

    PairT ta tb
     -> do va <- newSeaVectors sz ta
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
    (U64 v, VBool False, BoolT)     -> pure svs <* liftIO (MV.write v ix 0)
    (U64 v, VBool  True, BoolT)     -> pure svs <* liftIO (MV.write v ix 1)
    (I64 v, VInt      x, IntT)      -> pure svs <* liftIO (MV.write v ix (fromIntegral x))
    (F64 v, VDouble   x, DoubleT)   -> pure svs <* liftIO (MV.write v ix x)
    (U64 v, VDateTime x, DateTimeT) -> pure svs <* liftIO (MV.write v ix (packedOfDate x))
    (U64 v, VError    x, ErrorT)    -> pure svs <* liftIO (MV.write v ix (wordOfError x))

    (P64 v, VString xs, StringT)
     -> do let str = T.unpack xs
           ptr <- ptrToWordPtr <$> liftIO (newCString str)
           liftIO (MV.write v ix ptr)
           pure svs

    (P64 v, VArray xs, ArrayT tx)
     -> do ptr :: Ptr Word64 <- liftIO (mallocWords (length xs + 1))
           pokeArray ptr tx xs
           liftIO (MV.write v ix (ptrToWordPtr ptr))
           pure svs

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
     -> do svs1 <- pokeInput' svs0 BoolT ix (VBool False)
           svs2 <- pokeInput' svs1 ta    ix a
           svs3 <- pokeInput' svs2 tb    ix (defaultOfType tb)
           pure svs3

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

peekOutputs :: MonadIO m
            => Ptr a
            -> Int
            -> [(OutputName, (ValType, [ValType]))]
            -> EitherT SeaError m [(OutputName, BaseValue)]

peekOutputs _ _ []                     = pure []
peekOutputs ptr ix ((n, (t, _)) : ots) = do
  nvs    <- peekOutputs ptr (ix+1) ots
  (_, v) <- peekOutput  ptr ix t
  pure ((n, v) : nvs)


peekOutput :: MonadIO m => Ptr a -> Int -> ValType -> EitherT SeaError m (Int, BaseValue)
peekOutput ptr ix0 t =
  case t of
    UnitT     -> (ix0+1,)                            <$> pure VUnit
    IntT      -> (ix0+1,) . VInt      . fromInt64    <$> peekWordOff ptr ix0
    DoubleT   -> (ix0+1,) . VDouble                  <$> peekWordOff ptr ix0
    DateTimeT -> (ix0+1,) . VDateTime . dateOfPacked <$> peekWordOff ptr ix0
    ErrorT    -> (ix0+1,) . VError    . errorOfWord  <$> peekWordOff ptr ix0

    MapT{}    -> left (SeaTypeConversionError t)
    StructT{} -> left (SeaTypeConversionError t)
    BufT{}    -> left (SeaTypeConversionError t)

    StringT
     -> do strPtr <- wordPtrToPtr <$> peekWordOff ptr ix0
           str    <- liftIO (peekCString strPtr)
           pure (ix0+1, VString (T.pack str))

    ArrayT tx
     | StringT <- tx
     -> left (SeaTypeConversionError t)

     | otherwise
     -> do arrPtr :: Ptr Word64 <- wordPtrToPtr <$> peekWordOff ptr ix0
           xs <- peekArray arrPtr tx
           pure (ix0+1, VArray xs)

    BoolT
     -> do b <- peekWordOff ptr ix0
           case b :: Word64 of
             0 -> pure (ix0+1, VBool False)
             _ -> pure (ix0+1, VBool True)

    PairT ta tb
     -> do (ix1, va) <- peekOutput ptr ix0 ta
           (ix2, vb) <- peekOutput ptr ix1 tb
           pure (ix2, VPair va vb)

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
    (VBool False, BoolT)     -> liftIO (pokeWordOff ptr ix (0 :: Word64))
    (VBool  True, BoolT)     -> liftIO (pokeWordOff ptr ix (1 :: Word64))
    (VInt      x, IntT)      -> liftIO (pokeWordOff ptr ix (fromIntegral x :: Int64))
    (VDouble   x, DoubleT)   -> liftIO (pokeWordOff ptr ix x)
    (VDateTime x, DateTimeT) -> liftIO (pokeWordOff ptr ix (packedOfDate x))
    (VError    x, ErrorT)    -> liftIO (pokeWordOff ptr ix (wordOfError x))
    _                        -> left (SeaBaseValueConversionError v (Just t))

peekArray :: MonadIO m => Ptr x -> ValType -> EitherT SeaError m [BaseValue]
peekArray ptr t = do
  len <- peekWordOff ptr 0
  traverse (peekArrayIx ptr t) [1..len]

peekArrayIx :: MonadIO m => Ptr x -> ValType -> Int -> EitherT SeaError m BaseValue
peekArrayIx ptr t ix =
  case t of
    IntT      -> VInt      . fromInt64    <$> peekWordOff ptr ix
    DoubleT   -> VDouble                  <$> peekWordOff ptr ix
    DateTimeT -> VDateTime . dateOfPacked <$> peekWordOff ptr ix
    ErrorT    -> VError    . errorOfWord  <$> peekWordOff ptr ix

    BoolT
     -> do b <- peekWordOff ptr ix
           case b :: Word64 of
             0 -> pure (VBool False)
             _ -> pure (VBool True)

    _
     -> left (SeaTypeConversionError t)

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

withCString :: (MonadIO m, MonadMask m) => String -> (CString -> EitherT SeaError m a) -> EitherT SeaError m a
withCString str io =
  bracketEitherT' (liftIO (newCString str)) (liftIO . free) io

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
  ExceptTombstone                  -> 0
  ExceptFold1NoValue               -> 1
  ExceptScalarVariableNotAvailable -> 2

errorOfWord :: Word64 -> ExceptionInfo
errorOfWord = \case
  0 -> ExceptTombstone
  1 -> ExceptFold1NoValue
  2 -> ExceptScalarVariableNotAvailable
  _ -> ExceptTombstone
