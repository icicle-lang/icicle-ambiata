{-# LANGUAGE NoImplicitPrelude #-}

import           System.Environment               (getArgs)
import           System.IO

import           P

import qualified Icicle.Repl.Base   as Base
import           Icicle.Repl


main :: IO ()
main = getArgs >>= Base.runRepl defaultState handleLine

