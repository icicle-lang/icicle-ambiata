{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Data.Monoid
import           Data.List                            (words, replicate)
import           Data.String                          (String)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Traversable                     as TR
import           System.Console.Haskeline             as HL
import qualified System.Console.Terminal.Size         as TS
import           System.Directory
import           System.Environment                   (getArgs)
import           System.IO
import qualified Text.PrettyPrint.Leijen              as PP
import qualified Text.ParserCombinators.Parsec        as Parsec

import qualified Icicle.Avalanche.FromCore            as AC
import qualified Icicle.Avalanche.Prim.Flat           as APF
import qualified Icicle.Avalanche.Program             as AP
import qualified Icicle.Avalanche.Simp                as AS
import qualified Icicle.Avalanche.Statement.Flatten   as AF
import qualified Icicle.Avalanche.ToJava              as AJ
import qualified Icicle.Common.Base                   as CommonBase
import qualified Icicle.Common.Fresh                  as F
import qualified Icicle.Core.Program.Check            as CP
import qualified Icicle.Core.Program.Program          as CP
import qualified Icicle.Core.Exp.Prim                 as CP
import           Icicle.Data
import           Icicle.Data.DateTime
import           Icicle.Dictionary
import           Icicle.Internal.Rename
import qualified Icicle.Repl                          as SR
import qualified Icicle.Simulator                     as S
import qualified Icicle.Source.Parser                 as SP
import qualified Icicle.Source.PrettyAnnot            as SPretty
import qualified Icicle.Source.Query                  as SQ
import qualified Icicle.Source.Type                   as ST


import           P


main :: IO ()
main
 = do   as <- getArgs
        runRepl as

runRepl :: [String] -> IO ()
runRepl inits
  = do putStrLn "welcome to iREPL"
       s <- settings
       HL.runInputT s
        $ do    state' <- foldM handleLine defaultState inits
                loop state'
  where
    settings
      = do home <- getHomeDirectory
           return $ HL.defaultSettings
             { historyFile    = Just $ home <> "/.icicle-repl.history"
             , autoAddHistory = True}
    loop state
      = do line <- HL.getInputLine "> "
           case line of
             Nothing      -> return ()
             Just ":quit" -> return ()
             Just ":q"    -> return ()
             Just str     -> handleLine state str >>= loop

--------------------------------------------------------------------------------

data ReplState
   = ReplState
   { facts        :: [AsAt Fact]
   , dictionary   :: Dictionary
   , currentDate  :: DateTime
   , hasType      :: Bool
   , hasAnnotated :: Bool
   , hasCore      :: Bool
   , hasCoreType  :: Bool
   , hasAvalanche :: Bool
   , hasFlatten   :: Bool
   , hasJava      :: Bool
   , hasEval      :: Bool
   , doCoreSimp   :: Bool }

-- | Settable REPL states
data Set
   = ShowType           Bool
   | ShowAnnotated      Bool
   | ShowCore           Bool
   | ShowCoreType       Bool
   | ShowEval           Bool
   | ShowAvalanche      Bool
   | ShowFlatten        Bool
   | ShowJava           Bool
   | CurrentDate        DateTime
   | PerformCoreSimp    Bool

-- | REPL commands
data Command
   = CommandBlank
   | CommandHelp
   | CommandSet  [Set]
   | CommandLoad FilePath
   | CommandLoadDictionary FilePath
   -- It's rather odd to have comments in a REPL.
   -- However, I want these printed out in the test output
   | CommandComment String
   | CommandUnknown String
   | CommandSetShow

defaultState :: ReplState
defaultState
  = (ReplState [] demographics (dateOfYMD 1970 1 1) False False False False False False False False False)
    { hasEval = True }

readCommand :: String -> Maybe Command
readCommand ss = case words ss of
  []                    -> Just CommandBlank
  ":h":_                -> Just CommandHelp
  ":help":_             -> Just CommandHelp

  [":set"]              -> Just $ CommandSetShow
  (":set":rest)         -> CommandSet <$> readSetCommands rest
  [":load", f]          -> Just $ CommandLoad f
  [":dictionary", f]    -> Just $ CommandLoadDictionary f
  ('-':'-':_):_         -> Just $ CommandComment $ ss
  (':':_):_             -> Just $ CommandUnknown $ ss
  _                     -> Nothing

readSetCommands :: [String] -> Maybe [Set]
readSetCommands ss
 = case ss of
    ("+type":rest)      -> (:) (ShowType True)        <$> readSetCommands rest
    ("-type":rest)      -> (:) (ShowType False)       <$> readSetCommands rest

    ("+annotated":rest) -> (:) (ShowAnnotated True)   <$> readSetCommands rest
    ("-annotated":rest) -> (:) (ShowAnnotated False)  <$> readSetCommands rest

    ("+core":rest)      -> (:) (ShowCore True)        <$> readSetCommands rest
    ("-core":rest)      -> (:) (ShowCore False)       <$> readSetCommands rest

    ("+core-type":rest) -> (:) (ShowCoreType True)    <$> readSetCommands rest
    ("-core-type":rest) -> (:) (ShowCoreType False)   <$> readSetCommands rest

    ("+core-simp":rest) -> (:) (PerformCoreSimp True) <$> readSetCommands rest
    ("-core-simp":rest) -> (:) (PerformCoreSimp False)<$> readSetCommands rest

    ("+eval":rest)      -> (:) (ShowEval True)        <$> readSetCommands rest
    ("-eval":rest)      -> (:) (ShowEval False)       <$> readSetCommands rest

    ("+avalanche":rest) -> (:) (ShowAvalanche True)   <$> readSetCommands rest
    ("-avalanche":rest) -> (:) (ShowAvalanche False)  <$> readSetCommands rest

    ("+flatten":rest)   -> (:) (ShowFlatten   True)   <$> readSetCommands rest
    ("-flatten":rest)   -> (:) (ShowFlatten   False)  <$> readSetCommands rest

    ("+java":rest)      -> (:) (ShowJava      True)   <$> readSetCommands rest
    ("-java":rest)      -> (:) (ShowJava      False)  <$> readSetCommands rest

    ("date" : y : m : d : rest)
       | Just y' <- readMaybe y
       , Just m' <- readMaybe m
       , Just d' <- readMaybe d
       -> (:) (CurrentDate $ dateOfYMD y' m' d')      <$> readSetCommands rest

    []                  -> Just []
    _                   -> Nothing


handleLine :: ReplState -> String -> HL.InputT IO ReplState
handleLine state line = case readCommand line of
  Just CommandBlank          -> do
    return state
  Just (CommandUnknown s)    -> do
    HL.outputStrLn $ "unknown command '" <> s <> "'"
    HL.outputStrLn $ "use :h for help"
    return state
  Just CommandHelp           -> do
    usage
    return state

  Just CommandSetShow        -> do
    showState state
    return state

  Just (CommandSet sets)    -> foldM handleSetCommand state sets

  Just (CommandLoad fp)      -> do
    s  <- liftIO $ T.readFile fp
    case SR.readFacts (dictionary state) s of
      Left e   -> prettyHL e >> return state
      Right fs -> do
        HL.outputStrLn $ "ok, loaded " <> fp <> ", " <> show (length fs) <> " rows"
        return $ state { facts = fs }

  Just (CommandLoadDictionary fp) -> do
    s  <- liftIO $ T.readFile fp
    case SR.readDictionary s of
      Left e   -> prettyHL e >> return state
      Right d@(Dictionary ds) -> do
        HL.outputStrLn $ "ok, loaded dictionary " <> fp <> ", with " <> show (length ds) <> " features"
        return $ state { dictionary = d }

  Just (CommandComment comment) -> do
    HL.outputStrLn comment
    return state


  -- We use the simulator to evaluate the Icicle expression.
  Nothing -> do

    let hoist c = hoistEither c
    let prettyOut setting heading p
            = lift
            $ when (setting state)
            $ do    HL.outputStrLn heading
                    prettyHL p
                    nl

    checked <- runEitherT $ do
      parsed    <- hoist $ SR.sourceParse (T.pack line)
      (annot, typ)
                <- hoist $ SR.sourceCheck (dictionary state) parsed

      prettyOut hasType "- Type:" typ

      prettyOut hasAnnotated "- Annotated:" (SPretty.PrettyAnnot annot)

      core      <- hoist $ SR.sourceConvert (dictionary state) annot
      let core'  | doCoreSimp state
                 = renameP unVar $ SR.coreSimp core
                 | otherwise
                 = renameP unVar core

      prettyOut hasCore "- Core:" core'

      case CP.checkProgram core' of
       Left  e -> prettyOut (const True) "- Core type error:" e
       Right t -> prettyOut hasCoreType "- Core type:" t

      prettyOut hasAvalanche "- Avalanche:" (coreAvalanche core')

      let flat = coreFlatten core'
      case flat of
       Left  e -> prettyOut (const True) "- Flatten error:" e
       Right f
        -> do   prettyOut hasFlatten "- Flattened:" f
                prettyOut hasJava    "- Java:" (AJ.programToJava f)


      case coreEval (currentDate state) (facts state) annot core' of
       Left  e -> prettyOut hasEval "- Result error:" e
       Right r -> prettyOut hasEval "- Result:" r

      return ()

    case checked of
      Left  e -> prettyE e
      Right _ -> return ()

    return state

handleSetCommand :: ReplState -> Set -> HL.InputT IO ReplState
handleSetCommand state set
 = case set of
    ShowType b -> do
        HL.outputStrLn $ "ok, type is now " <> showFlag b
        return $ state { hasType = b }

    ShowAnnotated b -> do
        HL.outputStrLn $ "ok, annotated is now " <> showFlag b
        return $ state { hasAnnotated = b }

    ShowCore b -> do
        HL.outputStrLn $ "ok, core is now " <> showFlag b
        return $ state { hasCore = b }

    ShowCoreType b -> do
        HL.outputStrLn $ "ok, core-type is now " <> showFlag b
        return $ state { hasCoreType = b }

    ShowAvalanche b -> do
        HL.outputStrLn $ "ok, avalanche is now " <> showFlag b
        return $ state { hasAvalanche = b }

    ShowFlatten b -> do
        HL.outputStrLn $ "ok, flatten is now " <> showFlag b
        return $ state { hasFlatten = b }

    ShowJava b -> do
        HL.outputStrLn $ "ok, java is now " <> showFlag b
        return $ state { hasJava = b }

    ShowEval b -> do
        HL.outputStrLn $ "ok, eval is now " <> showFlag b
        return $ state { hasEval = b }

    CurrentDate d -> do
        HL.outputStrLn $ "ok, date set to " <> T.unpack (renderDate d)
        return $ state { currentDate = d }

    PerformCoreSimp b -> do
        HL.outputStrLn $ "ok, core-simp is now " <> showFlag b
        return $ state { doCoreSimp = b }

--------------------------------------------------------------------------------

type QueryTopPUV = SQ.QueryTop (ST.Annot SP.SourcePos SP.Variable) SP.Variable
type ProgramT    = CP.Program Text
newtype Result   = Result (Entity, Value)

instance PP.Pretty Result where
  pretty (Result (ent, val))
    = PP.pretty ent <> PP.comma <> PP.space <> PP.pretty val

unVar :: SP.Variable -> Text
unVar (SP.Variable t)  = t

coreEval :: DateTime -> [AsAt Fact] -> QueryTopPUV -> ProgramT
         -> Either SR.ReplError [Result]
coreEval d fs (renameQT unVar -> query) prog
  = let partitions = S.streams fs
        feat       = SQ.feature query
        result     = fmap (evalP feat) partitions
    in  mapLeft SR.ReplErrorRuntime
        . mapRight (fmap Result)
        . TR.sequenceA
        . fmap (justVal . fmap (fmap fst))
        . concat
        . filter (not . null)
        $ result

  where
    justVal (e, result) = fmap (e,) result

    evalP feat (S.Partition ent attr values)
      | CommonBase.Name feat' <- feat
      , attr == Attribute feat'= [(ent, evalV values)]
      | otherwise              = []

    evalV
      = S.evaluateVirtualValue prog d

-- | Converts Core to Avalanche then flattens the result.
--
coreFlatten :: ProgramT -> Either SR.ReplError (AP.Program Text APF.Prim)
coreFlatten prog
 = let av = coreAvalanche prog
   in   mapLeft  SR.ReplErrorFlatten
      . mapRight simpFlattened
      . mapRight (\(_,s') -> av { AP.statements = s' })
      $ F.runFreshT
      ( AF.flatten
      $ AP.statements av)
      (F.counterPrefixNameState (T.pack . show) "flat")

coreAvalanche :: ProgramT -> AP.Program Text CP.Prim
coreAvalanche prog
 = simpAvalanche
 $ AC.programFromCore (AC.namerText id) prog

simpAvalanche :: (Eq p, Show p) => AP.Program Text p -> AP.Program Text p
simpAvalanche av
 = let simp = AS.simpAvalanche av
       name = F.counterPrefixNameState (T.pack . show) "anf"
   in  snd $ F.runFresh simp name

simpFlattened :: AP.Program Text APF.Prim -> AP.Program Text APF.Prim
simpFlattened av
 = let simp = AS.simpFlattened av
       name = F.counterPrefixNameState (T.pack . show) "simp"
   in  snd $ F.runFresh (simp >>= AS.simpFlattened) name

--------------------------------------------------------------------------------

nl :: HL.InputT IO ()
nl = HL.outputStrLn ""

prettyE :: SR.ReplError -> HL.InputT IO ()
prettyE e
 = ppos >> HL.outputStrLn "REPL Error:" >> prettyHL e >> nl
 where
  ppos
   | Just sp <- SR.annotOfError e
   = HL.outputStrLn (replicate (Parsec.sourceColumn sp + 1) ' ' <> "\ESC[34mλλλλ\ESC[0m")
   | otherwise
   = return ()

prettyHL :: PP.Pretty a => a -> HL.InputT IO ()
prettyHL x
 = do   width <- terminalWidth
        let width' = maybe 80 id width
        HL.outputStrLn $ PP.displayS (PP.renderPretty 0.4 width' $ PP.pretty x) ""

terminalWidth :: HL.InputT IO (Maybe Int)
terminalWidth
 = fmap (fmap TS.width)
 $ liftIO TS.size

showFlag :: Bool -> String
showFlag True  = "on"
showFlag False = "off"


showState :: ReplState -> HL.InputT IO ()
showState state
 = mapM_ HL.outputStrLn
    [      "now:        " <> T.unpack (renderDate $ currentDate state)
    ,      "data:       " <> show (length $ facts state)
    ,      "dictionary: " <> show (dictionary state)
    , flag "type:       " hasType
    , flag "annotated:  " hasAnnotated
    , flag "core:       " hasCore
    , flag "core-type:  " hasCoreType
    , flag "core-simp:  " doCoreSimp
    , flag "eval:       " hasEval
    , flag "avalanche:  " hasAvalanche
    , flag "flatten:    " hasFlatten
    , flag "java:       " hasJava
    ]
 where
  flag nm setting
   = nm <> showFlag (setting state)


usage :: HL.InputT IO ()
usage
 = mapM_ HL.outputStrLn
      [ "Usage:"
      , ":help or :h        -- shows this message"
      , ":quit or :q        -- quits the REPL"
      , ":load <filepath>   -- loads a data set"
      , ":set  +/-type      -- whether to show the checked expression type"
      , ":set  +/-core      -- whether to show the Core conversion"
      , ":set  +/-core-type -- whether to show the Core conversion's type"
      , ":set  +/-core-simp -- whether to simplify the result of Core conversion"
      , ":set  +/-eval      -- whether to show the result"
      , ":set  +/-avalanche -- whether to show the Avalanche conversion"
      , ":set  +/-flatten   -- whether to show flattened Avalanche conversion"
      , ":set  +/-java      -- whether to show the Java result" ]

