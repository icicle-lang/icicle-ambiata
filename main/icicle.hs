{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class

import           Data.Text as T
import           Data.Text.IO as T

import           Icicle
import           Icicle.Data.DateTime

import qualified Icicle.Internal.Pretty as PP
-- import qualified Icicle.Core.Program.Program as Program
import qualified Icicle.Core.Program.Check   as Program

import           P

import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           X.Options.Applicative

main :: IO ()
main = getArgs >>= \args -> case args of
  ["-h"] ->
    usage
  ["--help"] ->
    usage
  ["--dictionary"] ->
     showDictionary demographics
  [factset] ->
    orDie renderParseError $ run demographics factset
  _ ->
    usage >> exitFailure


run :: Dictionary -> FilePath -> EitherT ParseError IO ()
run dict p =
  do    ls  <- lift
             $ T.lines <$> T.readFile p

        vs  <- hoistEither
             $ mapM (decodeEavt dict) ls

        let s = streams vs
        -- TODO add date as a command line argument
        let v = evaluateVirtuals dict (dateOfYMD 2015 1 1) s

        lift $ mapM_ print v

-- Show the virtual features
showDictionary :: Dictionary -> IO ()
showDictionary d
 = do   T.putStrLn "Virtual features:"
        let vs = getVirtualFeatures d
        mapM_ showVirtual vs
 where
  getVirtualFeatures (Dictionary fs)
   = P.concatMap getV fs

  getV (a, VirtualDefinition v)
   = [(a,v)]
  getV _
   = []

  showVirtual (a,v)
   = do T.putStrLn ("Name: "     <> getAttribute a)
        T.putStrLn ("Concrete: " <> getAttribute (concrete v))

        let prog = program v
        let check = Program.checkProgram prog

        T.putStrLn ("Program: ")
        print (PP.indent 4 $ PP.pretty prog)

        case check of
         Left err
          -> do T.putStrLn "Type error:"
                print (PP.indent 4 $ PP.pretty err)
         Right ty
          -> do T.putStrLn "Has type:"
                print (PP.indent 4 $ PP.pretty ty)


usage :: IO ()
usage = T.putStrLn . T.unlines $ [
    "icicle FACTSET"
  , ""
  , "  FACTSET     A path to a factset in textual EAVT format, currently"
  , "              it is expected that this factset would abide by the"
  , "              predifined 'demographics' dictionary."
  , ""
  , "icicle --dictionary"
  , ""
  , "              Show and typecheck the virtual features"
  ]
