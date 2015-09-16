{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Avalanche.Segfault (
    SegfaultError (..)
  , segfault
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT(..), hoistEither)

import           Data.Int (Int64)
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
import           Icicle.Avalanche.ToSea (seaOfProgram, outputsOfProgram)
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Type (ValType(..))
import           Icicle.Data
import           Icicle.Data.DateTime (daysOfDate)
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty)
import           Icicle.Internal.Pretty (pretty, text)
import           Icicle.Simulator

import           Jetski

import           P

import           System.IO (IO, putStrLn)

import           X.Control.Monad.Trans.Either (firstEitherT)

------------------------------------------------------------------------

data SegfaultError
   = SegfaultJetskiError          JetskiError
   | SegfaultValueConversionError Value
   | SegfaultTypeConversionError  ValType
   | SegfaultNoOutputs
   deriving (Eq, Show)

instance Pretty SegfaultError where
   pretty (SegfaultValueConversionError v) = text "Cannot convert: " <> pretty v
   pretty (SegfaultTypeConversionError  t) = text "Cannot convert: " <> pretty t
   pretty (SegfaultNoOutputs)              = text "No outputs"

   pretty (SegfaultJetskiError je) =
     case je of
       CompilerError _ _ stderr -> pretty stderr
       _                        -> pretty (show je)

------------------------------------------------------------------------

segfault :: (Show a, Show n, Pretty n, Ord n)
         => Program (Annot a) n Prim
         -> DateTime
         -> [AsAt Value]
         -> EitherT SegfaultError IO [(OutputName, Value)]

segfault program date values = do
    let dates = takeDates values
    facts <- hoistEither (takeIntFacts values)

    EitherT $
      allocaWords  20    $ \pState ->
      withArray    dates $ \pDates ->
      withArrayLen facts $ \count pFacts -> do

        forM_ [0..19] (\off -> pokeWordOff pState off (0 :: Word64))

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
            []         -> return (Left SegfaultNoOutputs)
            ((n, t):_) -> do
              ev <- liftIO (peekWordOff' t pState 5)
              return (fmap (\v -> [(n, v)]) ev)
  where
    options = [ "-O3", "-march=native" ]
    code    = textOfDoc (seaOfProgram program)

    squash (Left ee)          = Left (SegfaultJetskiError ee)
    squash (Right (Left ee))  = Left ee
    squash (Right (Right xx)) = Right xx

------------------------------------------------------------------------

allocaWords :: Int -> (Ptr a -> IO b) -> IO b
allocaWords n = allocaBytes (n*8)

pokeWordOff :: Storable a => Ptr x -> Int -> a -> IO ()
pokeWordOff ptr off x = pokeByteOff ptr (off*8) x

peekWordOff :: Storable a => Ptr x -> Int -> IO a
peekWordOff ptr off = peekByteOff ptr (off*8)

peekWordOff' :: ValType -> Ptr x -> Int -> IO (Either SegfaultError Value)
peekWordOff' typ ptr off
 = case typ of
    IntT    -> Right . IntValue . fromIntegral <$> (peekWordOff ptr off :: IO Int64)
    DoubleT -> Right . DoubleValue             <$> (peekWordOff ptr off :: IO Double)
    _       -> pure (Left (SegfaultTypeConversionError typ))

------------------------------------------------------------------------

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")

------------------------------------------------------------------------

takeDates :: [AsAt Value] -> [Word64]
takeDates = fmap (wordOfDate . time)

wordOfDate :: DateTime -> Word64
wordOfDate = fromIntegral . daysOfDate

takeIntFacts :: [AsAt Value] -> Either SegfaultError [Int64]
takeIntFacts = sequence . fmap (intOfValue . fact)

intOfValue :: Value -> Either SegfaultError Int64
intOfValue v
 = case v of
     IntValue x -> Right (fromIntegral x)
     _          -> Left (SegfaultValueConversionError v)
