{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icicle.Sea.Eval (
    SeaError (..)
  , seaEval
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT(..), hoistEither)

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import qualified Data.Map as Map

import           Foreign.C (CChar)
import           Foreign.Marshal (allocaBytes, withArray, withArrayLen)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(..))

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Type (ValType(..))
import           Icicle.Data
import           Icicle.Data.DateTime (daysOfDate)
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty)
import           Icicle.Internal.Pretty (pretty, text)
import           Icicle.Sea.FromAvalanche (seaOfProgram, stateWordsOfProgram, outputsOfProgram)

import           Jetski

import           P hiding (count)

import           System.IO (IO)

------------------------------------------------------------------------

data SeaError
   = SeaJetskiError          JetskiError
   | SeaValueConversionError Value
   | SeaTypeConversionError  ValType
   | SeaNoOutputs
   deriving (Eq, Show)

instance Pretty SeaError where
   pretty (SeaValueConversionError v) = text "Cannot convert: " <> pretty v
   pretty (SeaTypeConversionError  t) = text "Cannot convert: " <> pretty t
   pretty (SeaNoOutputs)              = text "No outputs"

   pretty (SeaJetskiError je) =
     case je of
       CompilerError _ _ stderr -> pretty stderr
       _                        -> pretty (show je)

------------------------------------------------------------------------

seaEval :: (Show a, Show n, Pretty n, Ord n)
         => Program (Annot a) n Prim
         -> DateTime
         -> [AsAt Value]
         -> EitherT SeaError IO [(OutputName, Value)]

seaEval program date values = do
    let words = stateWordsOfProgram program
        dates = takeDates values
    facts <- hoistEither (takeIntFacts values)

    EitherT $
      allocaWords  words $ \pState ->
      withArray    dates $ \pDates ->
      withArrayLen facts $ \count pFacts -> do

        -- clear the pState struct
        forM_ [0..(words-1)] $ \off ->
          pokeWordOff pState off (0 :: Word64)

        pokeWordOff pState 0 (wordOfDate date)
        pokeWordOff pState 1 (fromIntegral count :: Int64)
        pokeWordOff pState 2 pDates
        pokeWordOff pState 3 pFacts
        pokeWordOff pState 4 (nullPtr :: Ptr CChar)

        fmap squash . runEitherT
                    . withLibrary options code $ \lib -> do

          compute <- function lib "compute" retVoid
          _       <- liftIO (compute [argPtr pState])

          -- we should probably fix the order of this inside the ToSea module
          case Map.toList (outputsOfProgram program) of
            []              -> return (Left SeaNoOutputs)
            ((n, (t, _)):_) -> do
              ev <- liftIO (peekWordOff' t pState 5)
              return (fmap (\v -> [(n, v)]) ev)
  where
    options = [ "-O3"           -- 🔨
              , "-march=native" -- 🚀  all optimisations valid for the current CPU (AVX512, etc)
              , "-std=c99"      -- 👹  variable declarations anywhere!
              , "-fPIC"         -- 🌏  position independent code, required on Linux
              ]
    code    = textOfDoc (seaOfProgram program)

    squash (Left ee)          = Left (SeaJetskiError ee)
    squash (Right (Left ee))  = Left ee
    squash (Right (Right xx)) = Right xx

------------------------------------------------------------------------

allocaWords :: Int -> (Ptr a -> IO b) -> IO b
allocaWords n = allocaBytes (n*8)

pokeWordOff :: Storable a => Ptr x -> Int -> a -> IO ()
pokeWordOff ptr off x = pokeByteOff ptr (off*8) x

peekWordOff :: Storable a => Ptr x -> Int -> IO a
peekWordOff ptr off = peekByteOff ptr (off*8)

peekWordOff' :: ValType -> Ptr x -> Int -> IO (Either SeaError Value)
peekWordOff' typ ptr off
 = case typ of
    IntT    -> Right . IntValue . fromIntegral <$> (peekWordOff ptr off :: IO Int64)
    DoubleT -> Right . DoubleValue             <$> (peekWordOff ptr off :: IO Double)
    _       -> pure (Left (SeaTypeConversionError typ))

------------------------------------------------------------------------

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")

------------------------------------------------------------------------

takeDates :: [AsAt Value] -> [Word64]
takeDates = fmap (wordOfDate . time)

wordOfDate :: DateTime -> Word64
wordOfDate = fromIntegral . daysOfDate

takeIntFacts :: [AsAt Value] -> Either SeaError [Int64]
takeIntFacts = sequence . fmap (intOfValue . fact)

intOfValue :: Value -> Either SeaError Int64
intOfValue v
 = case v of
     IntValue x -> Right (fromIntegral x)
     _          -> Left (SeaValueConversionError v)
