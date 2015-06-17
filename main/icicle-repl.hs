{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

import           Control.Monad               (when)
import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Traversable            as TR
import           System.Console.Haskeline    as HL
import qualified Text.PrettyPrint.Leijen     as PP

import           Icicle.BubbleGum
import qualified Icicle.Core.Program.Program as CP
import           Icicle.Data
import           Icicle.Data.DateTime
import           Icicle.Dictionary
import           Icicle.Internal.Rename
import qualified Icicle.Repl                 as SR
import qualified Icicle.Simulator            as S
import qualified Icicle.Source.Parser        as SP
import qualified Icicle.Source.Query         as SQ
import qualified Icicle.Source.Type          as ST


main :: IO ()
main = runRepl

runRepl :: IO ()
runRepl
  = do putStrLn "welcome to iREPL"
       HL.runInputT HL.defaultSettings $ loop defaultState
  where
    loop :: ReplState -> HL.InputT IO ()
    loop state
      = do line <- HL.getInputLine "> "
           case line of
             Nothing      -> loop state
             Just ":quit" -> return ()
             Just ":q"    -> return ()
             Just str     -> handleLine state str >>= loop

--------------------------------------------------------------------------------

data ReplState
   = ReplState
   { facts   :: [AsAt Fact]
   , hasType :: Bool
   , hasCore :: Bool
   , hasEval :: Bool }

-- | Settable REPL states
data Set
   = ShowType
   | ShowCore
   | ShowEval

-- | REPL commands
data Command
   = CommandBlank
   | CommandSet  Set
   | CommandLoad FilePath

defaultState :: ReplState
defaultState
  = ReplState [] True True True

readCommand :: String -> Maybe Command
readCommand (words -> ss)
  | null ss                    = Just CommandBlank
  | ":set":"show-type":_ <- ss = Just $ CommandSet ShowType
  | ":set":"show-core":_ <- ss = Just $ CommandSet ShowCore
  | ":set":"show-eval":_ <- ss = Just $ CommandSet ShowEval
  | ":load":f:_          <- ss = Just $ CommandLoad f
  | otherwise                  = Nothing

handleLine :: ReplState -> String -> HL.InputT IO ReplState
handleLine state line = case readCommand line of
  Just CommandBlank          -> do
    HL.outputStrLn "please input a command or an Icicle expression"
    return state

  -- Whether to show type of result
  Just (CommandSet ShowType) -> do
    let v = not (hasType state)
    HL.outputStrLn $ concat ["ok, show-type is now ", showFlag v]
    return $ state { hasType = v }

  -- Whether to show core
  Just (CommandSet ShowCore) -> do
    let v = not (hasCore state)
    HL.outputStrLn $ concat ["ok, show-core is now ", showFlag v]
    return $ state { hasCore = v }

  -- Whether to show eval result
  Just (CommandSet ShowEval) -> do
    let v = not (hasEval state)
    HL.outputStrLn $ concat ["ok, show-eval is now ", showFlag v]
    return $ state { hasEval = v }

  Just (CommandLoad fp)      -> do
    s  <- liftIO $ T.readFile fp
    case SR.readFacts dict s of
      Left e   -> prettyHL e >> return state
      Right fs -> do
        HL.outputStrLn "ok, loaded"
        return $ state { facts = fs }

  -- An Icicle expression
  -- we use the core evaluator instead of the source evaluator here
  -- since it's more complete at the moment.
  Nothing -> do
    let checked = do
          qt     <- SR.sourceParse (T.pack line)
          (q, u) <- SR.sourceCheck dict qt
          p      <- SR.sourceConvert q
          return (q, u, p)

    case checked of
      Left  e      -> prettyE e
      Right (q, u, p) -> do
        when (hasType state) $ HL.outputStrLn "- Type:" >> prettyHL u >> nl
        when (hasCore state) $ HL.outputStrLn "- Core:" >> prettyHL p >> nl
        when (hasEval state) $ case coreEval allTime (facts state) q p of
          Left  e -> prettyE e
          Right r -> HL.outputStrLn "- Result:" >> prettyHL r >> nl

    return state

  where
    -- todo load dictionary
    dict = demographics
    -- todo let user specify window
    allTime = dateOfYMD 1970 1 1

--------------------------------------------------------------------------------

-- TODO these marshalling belongs functions somehere

type QueryTopPUV = SQ.QueryTop (SP.SourcePos, ST.UniverseType) SP.Variable
type ProgramV    = CP.Program SP.Variable

unVar :: SP.Variable -> Text
unVar (SP.Variable t)  = t

coreEval
  :: DateTime
  -> [AsAt Fact]
  -> QueryTopPUV
  -> ProgramV
  -> Either SR.ReplError [(Entity, Value)]
coreEval d fs (renameQT unVar -> query) (renameP unVar -> prog)
  = let partitions = S.streams fs
        feat       = SQ.feature query
        result     = map (evalP feat) partitions
    in  mapLeft SR.ReplErrorRuntime
        . TR.sequenceA
        . map (justVal . fmap (fmap fst))
        . concat
        . filter (not . null)
        $ result

  where
    justVal (e, result) = fmap (e,) result

    evalP feat (S.Partition ent attr values)
      | attr == Attribute feat = [(ent, evalV values)]
      | otherwise              = []

    evalV
      = S.evaluateVirtualValue prog d

--------------------------------------------------------------------------------

nl :: HL.InputT IO ()
nl = HL.outputStrLn ""

prettyE :: SR.ReplError -> HL.InputT IO ()
prettyE e = HL.outputStrLn "REPL Error:" >> prettyHL e >> nl

prettyHL :: PP.Pretty a => a -> HL.InputT IO ()
prettyHL x = HL.outputStrLn $ PP.displayS (PP.renderPretty 0.4 100 $ PP.pretty x) ""

showFlag :: Bool -> String
showFlag True  = "on"
showFlag False = "off"

instance PP.Pretty (Entity, Value) where
  pretty (ent, val) = PP.pretty ent <> PP.comma <> PP.space <> PP.pretty val
