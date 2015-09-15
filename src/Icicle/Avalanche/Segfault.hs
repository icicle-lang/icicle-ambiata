{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Avalanche.Segfault (
    segfault
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT)

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)

import           Foreign.C (CChar)
import           Foreign.Marshal (allocaBytes)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(..))

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.ToSea (seaOfProgram)
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Data
import           Icicle.Internal.Pretty (Doc, Pretty, displayS, renderPretty, pretty)
import           Icicle.Simulator

import           Jetski

import           P

import           System.IO (IO, putStrLn)

import           X.Control.Monad.Trans.Either (firstEitherT)

------------------------------------------------------------------------

data SegfaultError
   = JetskiError JetskiError
   deriving (Eq, Show)

instance Pretty SegfaultError where
   pretty (JetskiError e) = pretty (show e)

------------------------------------------------------------------------

segfault :: (Show a, Show n, Pretty n, Ord n)
         => Program (Annot a) n Prim
         -> DateTime
         -> [AsAt Fact]
         -> EitherT SegfaultError IO [(OutputName, Value)]

segfault program date values =
  firstEitherT JetskiError . withLibrary options code $ \lib -> do
    compute <- function lib "compute" retVoid
    let count = 10
    liftIO $
      allocaWords 20    $ \pState ->
      allocaWords count $ \pDates ->
      allocaWords count $ \pFacts -> do

        putStrLn (show values)

        forM_ [0..19]    (\off -> pokeWordOff pState off (0 :: Word64))
        forM_ [0..count] (\off -> pokeWordOff pDates off (fromIntegral off :: Word64))
        forM_ [0..count] (\off -> pokeWordOff pFacts off (fromIntegral off :: Int64))

        let gen_date  = 0       :: Word64
            new_count = fromIntegral count :: Int64
            new_date  = pDates
            new_fact  = pFacts
            out_error = nullPtr :: Ptr CChar
            out_repl  = 0.0     :: Double

        pokeWordOff pState 0 gen_date
        pokeWordOff pState 1 new_count
        pokeWordOff pState 2 new_date
        pokeWordOff pState 3 new_fact
        pokeWordOff pState 4 out_error
        pokeWordOff pState 5 out_repl

        _ <- compute [argPtr pState]

        whee <- peekWordOff pState 5

        return [(OutputName "repl", DoubleValue whee)]
  where
    options = [ "-O3", "-march=native" ]
    code    = textOfDoc (seaOfProgram program)

------------------------------------------------------------------------

allocaWords :: Int -> (Ptr a -> IO b) -> IO b
allocaWords n = allocaBytes (n*8)

pokeWordOff :: Storable a => Ptr b -> Int -> a -> IO ()
pokeWordOff ptr off x = pokeByteOff ptr (off*8) x

peekWordOff :: Storable a => Ptr b -> Int -> IO a
peekWordOff ptr off = peekByteOff ptr (off*8)

------------------------------------------------------------------------

textOfDoc :: Doc -> Text
textOfDoc doc = T.pack (displayS (renderPretty 0.8 80 (pretty doc)) "")

--evaluateVirtualValue :: P.Program a Text -> DateTime -> [AsAt Value] -> Result a

--evaluateVirtualValue' :: A.Program a Text XP.Prim -> DateTime -> [AsAt Value] -> Result a
--evaluateVirtualValue' p date vs
-- = do   vs' <- mapM toCore vs
--
--        xv  <- mapLeft SimulateErrorRuntime'
--             $ AE.evalProgram XV.evalPrim date vs' p
--
--        v'  <- mapM (\(n,v) -> (,) n <$> valueFromCore v) $ snd xv
--        bg' <- mapM (B.mapValue valueFromCore) (fst xv)
--        return (v', bg')
-- where
--  toCore a
--   = do v' <- valueToCore $ fact a
--        return a { fact = (B.BubbleGumFact $ B.Flavour 0 $ time a, v') }
