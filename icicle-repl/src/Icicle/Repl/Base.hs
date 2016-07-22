{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternGuards     #-}

module Icicle.Repl.Base
  ( Command (..)
  , DictionaryLoadType (..)
  , readCommand
  , runRepl
  , nl
  , renderReplError
  , prettyHL
  , terminalWidth
  , showFlag
  ) where

import           Control.Monad.IO.Class

import           Data.Monoid

import qualified System.Console.ANSI              as ANSI
import           System.Console.Haskeline         as HL
import qualified System.Console.Terminal.Size     as TS
import           System.Directory

import qualified Text.ParserCombinators.Parsec    as Parsec

import           P

import           Icicle.Internal.Pretty           (Pretty)
import qualified Icicle.Internal.Pretty           as PP


data Command setcmd
   = CommandBlank
   | CommandHelp
   | CommandSet            [setcmd]
   | CommandLoad           FilePath
   | CommandLoadDictionary DictionaryLoadType
   | CommandImportLibrary  FilePath
   | CommandComment        String
   | CommandUnknown        String
   | CommandSetShow


data DictionaryLoadType
 = DictionaryLoadTextV1 FilePath
 | DictionaryLoadToml   FilePath
 deriving (Eq, Ord, Show)


readCommand :: ([String] -> Maybe [a])
            -> String
            -> Maybe (Command a)
readCommand readSetCommands ss = case words ss of
  []                               -> Just CommandBlank
  ":h":_                           -> Just CommandHelp
  ":help":_                        -> Just CommandHelp
  [":set"]                         -> Just $ CommandSetShow
  (":set":rest)                    -> CommandSet <$> readSetCommands rest
  [":load", f]                     -> Just $ CommandLoad f
  [":dictionary-deprecated", f]    -> Just $ CommandLoadDictionary $ DictionaryLoadTextV1 f
  [":dictionary", f]               -> Just $ CommandLoadDictionary $ DictionaryLoadToml f
  [":import", f]                   -> Just $ CommandImportLibrary f
  ('-':'-':_):_                    -> Just $ CommandComment $ ss
  (':':_):_                        -> Just $ CommandUnknown $ ss
  _                                -> Nothing


runRepl :: a
        -> (a -> String -> HL.InputT IO a)
        -> [String] -> IO ()
runRepl defaultState handleLine inits
  = do putStrLn "welcome to iREPL"
       h <- getHomeDirectory
       c <- getCurrentDirectory
       s <- settings h
       HL.runInputT s
        $ do dot1   <- liftIO $ dotfile (h <> "/.icicle")
             dot2   <- liftIO $ dotfile (c <> "/.icicle")
             state' <- foldM handleLine defaultState (dot1 <> dot2 <> inits)
             withInterrupt $ loop state'
  where
    settings home
      = return $ HL.defaultSettings
          { historyFile    = Just $ home <> "/.icicle-repl.history"
          , autoAddHistory = True}
    dotfile fp
      = do b <- doesFileExist fp
           if b
           then lines <$> readFile fp
           else return []
    loop state
      = handleInterrupt (loop state)
      $ do line <- HL.getInputLine "> "
           case line of
             Nothing      -> return ()
             Just ":quit" -> return ()
             Just ":q"    -> return ()
             Just str     -> handleLine state str >>= loop

--------------------------------------------------------------------------------

nl :: HL.InputT IO ()
nl = HL.outputStrLn ""

renderReplError :: Pretty err
                => err
                -> (err -> Maybe Parsec.SourcePos)
                -> HL.InputT IO ()
renderReplError e errorPos
 = ppos >> HL.outputStrLn "REPL Error:" >> prettyHL e >> nl
 where
  ppos
   | Just sp <- errorPos e
   = HL.outputStrLn
   $ replicate (Parsec.sourceColumn sp + 1) ' '
     <> ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
     <> "λλλλ"
     <> ANSI.setSGRCode [ANSI.Reset]
   | otherwise
   = return ()

prettyHL :: Pretty a => a -> HL.InputT IO ()
prettyHL x
 = do   width <- terminalWidth
        let width' = maybe 80 id width
        HL.outputStrLn $ PP.displayDecorated withColour (PP.renderPretty 0.4 width' $ PP.pretty x)
    where
      withColour a'@(PP.AnnVariable) str = sgrAttr a' <> str <> sgrReset
      withColour a'@(PP.AnnType a)   str = str <> sgrAttr a' <> (concat . lines . PP.display . PP.renderCompact $ ("@" <> PP.braces (PP.pretty a))) <> sgrReset

      sgrReset = ANSI.setSGRCode [ANSI.Reset]

      sgrAttr = \case
        PP.AnnVariable    -> ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        PP.AnnType _      -> ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]


terminalWidth :: HL.InputT IO (Maybe Int)
terminalWidth
 = fmap (fmap TS.width)
 $ liftIO TS.size

showFlag :: Bool -> String
showFlag True  = "on"
showFlag False = "off"
