{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class

import           Data.Text as T
import           Data.Text.IO as T

import           Icicle
import           Icicle.Data.DateTime

import qualified Icicle.Internal.Pretty as PP
import qualified Icicle.Core.Program.Check   as Program

import qualified Icicle.Avalanche.FromCore   as AvC
import qualified Icicle.Avalanche.Simp       as AvS

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

        lift $ mapM_ (print.prettyResult) v
 where
  prettyResult (attr, vals)
   =      PP.text "Virtual feature: " <> PP.text (T.unpack $ getAttribute attr)
   PP.<$> PP.indent 4 (PP.vcat $ fmap prettyResultEnt vals)

  prettyResultEnt (ent, res)
   =      PP.text "Entity:  " <> PP.text (show $ getEntity ent)
   PP.<$> case res of
            Left e
                -> PP.text "Error:   " PP.<$> PP.indent 4 (PP.text $ show e) <> PP.line
            Right (v,hist)
                -> PP.text "Value:   " <> PP.text (show v) PP.<$> PP.text "History: " <> PP.indent 0 (PP.vcat $ fmap (PP.text . show) hist) <> PP.line

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
   = do T.putStrLn ("Name:     "     <> getAttribute a)
        T.putStrLn ("Concrete: " <> getAttribute (concrete v))

        let prog = program v
        let check = Program.checkProgram prog

        T.putStrLn ("Program:  ")
        print (PP.indent 4 $ PP.pretty prog)

        case check of
         Left err
          -> do T.putStrLn "Type error:"
                print (PP.indent 4 $ PP.pretty err)
         Right ty
          -> do T.putStrLn "Has type:"
                print (PP.indent 4 $ PP.pretty ty)
         
        let av = AvS.simpAvalanche $ AvC.programFromCore (AvC.namerText id) prog
        T.putStrLn "Avalanche:"
        print (PP.indent 4 $ PP.pretty av)

        T.putStrLn ""



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
