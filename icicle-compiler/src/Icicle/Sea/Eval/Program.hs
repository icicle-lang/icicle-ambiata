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
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Map as Map
import           Data.Map (Map)

import           Foreign.Ptr (Ptr)

import           System.IO (IO)

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.Statement.Statement (FactLoopType(..))

import           Icicle.Data (InputId, OutputId)

import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Type (ValType(..))

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Analysis (factVarsOfProgram, outputsOfProgram)
import           Icicle.Sea.FromAvalanche.Program (nameOfCompute')
import           Icicle.Sea.FromAvalanche.State (nameOfStateSize')
import           Icicle.Sea.Fleet

import           P hiding (count)

import           Jetski

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, left)


data SeaProgram = SeaProgram {
    spName        :: (Int,Int)
  , spStateWords  :: Int
  , spFactType    :: ValType
  , spOutputs     :: [(OutputId, (ValType, [ValType]))]
  , spCompute     :: Ptr SeaState -> IO ()
  }

mkSeaPrograms ::
     (MonadIO m, Eq n)
  => Library
  -> Map InputId (NonEmpty (Program (Annot a) n Prim))
  -> EitherT SeaError m (Map InputId (NonEmpty SeaProgram))
mkSeaPrograms lib programs = do
  compiled <- zipWithM go [0..] (Map.elems programs)
  return $ Map.fromList (List.zip (Map.keys programs) compiled)
 where
  go i ps
   = mapM (\(j,p) -> mkSeaProgram lib (i,j) p) (NonEmpty.zip (0 :| [1..]) ps)


mkSeaProgram ::
     (MonadIO m, Eq n)
  => Library
  -> (Int,Int)
  -> Program (Annot a) n Prim
  -> EitherT SeaError m SeaProgram
mkSeaProgram lib name@(attribute,_) program = do
  let outputs = outputsOfProgram program

  factType <- case factVarsOfProgram FactLoopNew program of
                Nothing     -> left SeaNoFactLoop
                Just (t, _) -> return t

  size_of_state <- firstEitherT SeaJetskiError (function lib (nameOfStateSize' attribute) retInt64)
  words         <- liftIO (size_of_state [])
  compute       <- firstEitherT SeaJetskiError (function lib (nameOfCompute' name) retVoid)

  return SeaProgram {
      spName       = name
    , spStateWords = fromIntegral words
    , spFactType   = factType
    , spOutputs    = outputs
    , spCompute    = \ptr -> compute [argPtr ptr]
    }
