{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Dictionary.Types where

import Icicle.Dictionary

import Icicle.Core.Program.Check

import qualified    Icicle.Internal.Pretty as PP

import              P
import              System.IO

check_virtual prog
 = case checkProgram prog of
    Left err
     -> do  putStrLn "With program:"
            print (PP.pretty prog)
            putStrLn "Got typechecking error:"
            print (PP.pretty err)
            return False
    Right _
     -> return True

check_attributes (Dictionary attrs)
 = and <$> mapM check attrs
 where
  check (_, ConcreteDefinition _)
   = return True
  check (_, VirtualDefinition virtual)
   = check_virtual (program virtual)

tests :: IO Bool
tests
 = do   putStrLn "=== test/Icicle/Test/Dictionary/Types: typechecking demogrpahics === "
        res <- check_attributes demographics
        case res of
         True  -> putStrLn "+++ OK"
         False -> putStrLn "--- FAIL"
        return res
