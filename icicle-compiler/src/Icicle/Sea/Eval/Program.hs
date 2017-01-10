{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Eval.Program (
    SeaProgram (..)
  , mkSeaProgram
  , mkSeaPrograms
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)

import           Foreign.Ptr (Ptr)

import           System.IO (IO)

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.Statement.Statement (FactLoopType(..))

import           Icicle.Data (Attribute(..))

import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Type (ValType(..))

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Analysis (factVarsOfProgram, outputsOfProgram)
import           Icicle.Sea.FromAvalanche.Program (nameOfProgram')
import           Icicle.Sea.FromAvalanche.State (nameOfStateSize')
import           Icicle.Sea.Fleet

import           P hiding (count)

import           Jetski

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, left)


data SeaProgram = SeaProgram {
    spName        :: Int
  , spStateWords  :: Int
  , spFactType    :: ValType
  , spOutputs     :: [(OutputName, (ValType, [ValType]))]
  , spCompute     :: Ptr SeaState -> IO ()
  }

mkSeaPrograms ::
     (MonadIO m, Eq n)
  => Library
  -> Map Attribute (Program (Annot a) n Prim)
  -> EitherT SeaError m (Map Attribute SeaProgram)
mkSeaPrograms lib programs = do
  compiled <- zipWithM (mkSeaProgram lib) [0..] (Map.elems programs)
  return $ Map.fromList (List.zip (Map.keys programs) compiled)

mkSeaProgram ::
     (MonadIO m, Eq n)
  => Library
  -> Int
  -> Program (Annot a) n Prim
  -> EitherT SeaError m SeaProgram
mkSeaProgram lib name program = do
  let outputs = outputsOfProgram program

  factType <- case factVarsOfProgram FactLoopNew program of
                Nothing     -> left SeaNoFactLoop
                Just (t, _) -> return t

  size_of_state <- firstEitherT SeaJetskiError (function lib (nameOfStateSize' name) retInt64)
  words         <- liftIO (size_of_state [])
  compute       <- firstEitherT SeaJetskiError (function lib (nameOfProgram' name) retVoid)

  return SeaProgram {
      spName       = name
    , spStateWords = fromIntegral words
    , spFactType   = factType
    , spOutputs    = outputs
    , spCompute    = \ptr -> compute [argPtr ptr]
    }
