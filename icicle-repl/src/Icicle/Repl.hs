{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
module Icicle.Repl where

import           P

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import qualified Data.List                        as L
import qualified Data.Set                         as Set
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.String                      (String)

import           System.Console.Haskeline         as HL
import           System.IO

import qualified Text.ParserCombinators.Parsec    as Parsec

import           X.Control.Monad.Trans.Either


import           Icicle.Data
import           Icicle.Data.Time

import           Icicle.Dictionary
import           Icicle.Internal.Pretty           (Pretty, pretty)
import qualified Icicle.Internal.Pretty           as Pretty
import qualified Icicle.Simulator                 as Sim
import           Icicle.Storage.Dictionary.Toml

import qualified Icicle.Source.Checker            as Source
import qualified Icicle.Source.PrettyAnnot        as Source

import qualified Icicle.Core.Program.Check        as Core

import qualified Icicle.Avalanche.Simp            as Avalanche
import qualified Icicle.Avalanche.Annot           as Avalanche

import qualified Icicle.Sea.Eval                  as Sea
import qualified Icicle.Sea.FromAvalanche.Program as Sea
import qualified Icicle.Sea.Preamble              as Sea

import qualified Icicle.Compiler.Source           as Source
import qualified Icicle.Compiler                  as Compiler

import qualified Icicle.Repl.Base                 as Repl
import qualified Icicle.Repl.Source               as SourceRepl


--------------------------------------------------------------------------------

data ReplState
   = ReplState
   { sourceState        :: SourceRepl.SourceReplState
   , hasCore            :: Bool
   , hasCoreType        :: Bool
   , hasCoreEval        :: Bool
   , hasAvalanche       :: Bool
   , hasAvalancheEval   :: Bool
   , hasFlattenSimp     :: Bool
   , hasFlattenNoSimp   :: Bool
   , hasSeaPreamble     :: Bool
   , hasSea             :: Bool
   , hasSeaAssembly     :: Bool
   , hasSeaLLVMIR       :: Bool
   , hasSeaEval         :: Bool
   , doFlattenSimpCheck :: Bool
   , doCoreSimp         :: Bool }

defaultState :: ReplState
defaultState
  = (ReplState
      SourceRepl.defaultSourceReplState
      False False False False False False False False False False False False False False)
      { hasCoreEval = True
      , doCoreSimp  = True }


data Set
   = ShowType                Bool
   | ShowTypeCheckLog        Bool
   | ShowBigData             Bool
   | ShowAnnotated           Bool
   | ShowInlined             Bool
   | ShowDesugar             Bool
   | ShowReified             Bool
   | InlineOpt               Source.InlineOption
   | ShowCore                Bool
   | ShowCoreType            Bool
   | ShowCoreEval            Bool
   | ShowAvalanche           Bool
   | ShowAvalancheEval       Bool
   | ShowFlattenSimp         Bool
   | ShowFlattenNoSimp       Bool
   | ShowSeaPreamble         Bool
   | ShowSea                 Bool
   | ShowSeaAssembly         Bool
   | ShowSeaLLVMIR           Bool
   | ShowSeaEval             Bool
   | CurrentTime             Time
   | SetMaxMapSize           Int
   | PerformCoreSimp         Bool
   | PerformFlattenSimpCheck Bool


readSetCommands :: [String] -> Maybe [Set]
readSetCommands ss
 = case ss of
    ("+type":rest)               -> (:) (ShowType                True)  <$> readSetCommands rest
    ("-type":rest)               -> (:) (ShowType                False) <$> readSetCommands rest

    ("+type-check-log":rest)     -> (:) (ShowTypeCheckLog        True)  <$> readSetCommands rest
    ("-type-check-log":rest)     -> (:) (ShowTypeCheckLog        False) <$> readSetCommands rest

    ("+big-data":rest)           -> (:) (ShowBigData             True)  <$> readSetCommands rest
    ("-big-data":rest)           -> (:) (ShowBigData             False) <$> readSetCommands rest

    ("+annotated":rest)          -> (:) (ShowAnnotated           True)  <$> readSetCommands rest
    ("-annotated":rest)          -> (:) (ShowAnnotated           False) <$> readSetCommands rest

    ("+inlined":rest)            -> (:) (ShowInlined             True)  <$> readSetCommands rest
    ("-inlined":rest)            -> (:) (ShowInlined             False) <$> readSetCommands rest

    ("+desugar":rest)            -> (:) (ShowDesugar             True)  <$> readSetCommands rest
    ("-desugar":rest)            -> (:) (ShowDesugar             False) <$> readSetCommands rest

    ("+reified":rest)            -> (:) (ShowReified             True)  <$> readSetCommands rest
    ("-reified":rest)            -> (:) (ShowReified             False) <$> readSetCommands rest

    ("inline" : "with-lets" : rest)
       -> (:) (InlineOpt Source.InlineUsingLets)  <$> readSetCommands rest
    ("inline" : "with-subst" : rest)
       -> (:) (InlineOpt Source.InlineUsingSubst) <$> readSetCommands rest

    ("+core":rest)               -> (:) (ShowCore                True)  <$> readSetCommands rest
    ("-core":rest)               -> (:) (ShowCore                False) <$> readSetCommands rest

    ("+core-type":rest)          -> (:) (ShowCoreType            True)  <$> readSetCommands rest
    ("-core-type":rest)          -> (:) (ShowCoreType            False) <$> readSetCommands rest

    ("+core-simp":rest)          -> (:) (PerformCoreSimp         True)  <$> readSetCommands rest
    ("-core-simp":rest)          -> (:) (PerformCoreSimp         False) <$> readSetCommands rest

    ("+core-eval":rest)          -> (:) (ShowCoreEval            True)  <$> readSetCommands rest
    ("-core-eval":rest)          -> (:) (ShowCoreEval            False) <$> readSetCommands rest

    ("+avalanche":rest)          -> (:) (ShowAvalanche           True)  <$> readSetCommands rest
    ("-avalanche":rest)          -> (:) (ShowAvalanche           False) <$> readSetCommands rest

    ("+avalanche-eval":rest)     -> (:) (ShowAvalancheEval       True)  <$> readSetCommands rest
    ("-avalanche-eval":rest)     -> (:) (ShowAvalancheEval       False) <$> readSetCommands rest

    ("+flatten":rest)            -> (:) (ShowFlattenSimp         True)  <$> readSetCommands rest
    ("-flatten":rest)            -> (:) (ShowFlattenSimp         False) <$> readSetCommands rest

    ("+flatten-no-simp":rest)    -> (:) (ShowFlattenNoSimp       True)  <$> readSetCommands rest
    ("-flatten-no-simp":rest)    -> (:) (ShowFlattenNoSimp       False) <$> readSetCommands rest

    ("+flatten-simp":rest)       -> (:) (ShowFlattenSimp         True)  <$> readSetCommands rest
    ("-flatten-simp":rest)       -> (:) (ShowFlattenSimp         False) <$> readSetCommands rest

    ("+flatten-simp-check":rest) -> (:) (PerformFlattenSimpCheck True)  <$> readSetCommands rest
    ("-flatten-simp-check":rest) -> (:) (PerformFlattenSimpCheck False) <$> readSetCommands rest

    ("+c-preamble":rest)         -> (:) (ShowSeaPreamble         True)  <$> readSetCommands rest
    ("-c-preamble":rest)         -> (:) (ShowSeaPreamble         False) <$> readSetCommands rest

    ("+c":rest)                  -> (:) (ShowSea                 True)  <$> readSetCommands rest
    ("-c":rest)                  -> (:) (ShowSea                 False) <$> readSetCommands rest

    ("+c-assembly":rest)         -> (:) (ShowSeaAssembly         True)  <$> readSetCommands rest
    ("-c-assembly":rest)         -> (:) (ShowSeaAssembly         False) <$> readSetCommands rest

    ("+c-llvm-ir":rest)          -> (:) (ShowSeaLLVMIR           True)  <$> readSetCommands rest
    ("-c-llvm-ir":rest)          -> (:) (ShowSeaLLVMIR           False) <$> readSetCommands rest

    ("+c-eval":rest)             -> (:) (ShowSeaEval             True)  <$> readSetCommands rest
    ("-c-eval":rest)             -> (:) (ShowSeaEval             False) <$> readSetCommands rest

    ("time" : y : m : d : rest)
       | Just y' <- readMaybe y
       , Just m' <- readMaybe m
       , Just d' <- readMaybe d
       , Just x' <- timeOfYMD y' m' d'
       -> (:) (CurrentTime x') <$> readSetCommands rest

    ("max-map-size" : m : rest)
       | Just m' <- readMaybe m
       -> (:) (SetMaxMapSize m') <$> readSetCommands rest

    [] -> Just []
    _  -> Nothing


handleLine :: ReplState -> String -> HL.InputT IO ReplState
handleLine state line = let st = sourceState state in
 case Repl.readCommand readSetCommands line of
  Just Repl.CommandBlank  ->
    return state

  Just (Repl.CommandUnknown s) -> do
    HL.outputStrLn $ "unknown command '" <> s <> "'"
    HL.outputStrLn $ "use :h for help"
    return state

  Just Repl.CommandHelp -> do
    usage
    return state

  Just Repl.CommandSetShow -> do
    showState state
    return state

  Just (Repl.CommandLoad fp) -> do
    s  <- liftIO $ T.readFile fp
    case SourceRepl.readFacts (SourceRepl.dictionary st) s of
      Left e   ->
        Repl.prettyHL e >> return state
      Right fs -> do
        HL.outputStrLn $  "ok, loaded "
                       <> fp
                       <> ", "
                       <> show (length fs)
                       <> " rows"
        return $ state { sourceState = (st { SourceRepl.facts = fs }) }

  Just (Repl.CommandLoadDictionary load) -> do
    s  <- liftIO $ runEitherT $ SourceRepl.loadDictionary checkOpts load
    case s of
      Left e   -> Repl.prettyHL e >> return state
      Right d -> do
        HL.outputStrLn $  "ok, loaded dictionary with "
                       <> show (length $ dictionaryInputs d)
                       <> " inputs, "
                       <> show (length $ dictionaryOutputs d)
                       <> " outputs, "
                       <> show (length $ dictionaryFunctions d)
                       <> " functions"
        return $ state { sourceState = (st { SourceRepl.dictionary = d }) }

  Just (Repl.CommandImportLibrary fp) -> do
    s  <- liftIO $ T.readFile fp
    case SourceRepl.readIcicleLibrary fp s of
      Left e   -> Repl.prettyHL e >> return state
      Right is -> do
        HL.outputStrLn $ "ok, loaded " <> show (length is) <> " functions from " <> fp
        let d = SourceRepl.dictionary st
        -- Merge in the new functions with new functions taking precedence over existing ones
        let f = L.nubBy ((==) `on` functionName) $ fromFunEnv is <> dictionaryFunctions d
        return $ state { sourceState = st { SourceRepl.dictionary = d { dictionaryFunctions = f } } }

  Just (Repl.CommandComment comment) -> do
    HL.outputStrLn comment
    return state

  Just (Repl.CommandSet sets) ->
    foldM handleSetCommand state sets

  Just (Repl.CommandLetFunction funtext) -> withError $ do
    parsed <- hoist $ wrapSourceError $ Source.sourceParseF "repl" (T.pack funtext)
    let d      = SourceRepl.dictionary st
    let names  = Set.fromList $ fmap (snd . fst) parsed
    -- Remove the old bindings with these names
    let funEnv = filter (\(n,_) -> not $ Set.member n names)
               $ toFunEnv
               $ dictionaryFunctions d

    (funEnv',logs) <- hoist $ wrapSourceError $ Source.sourceCheckFunLog funEnv parsed
    let fundefs = filter (\(n,_) -> Set.member n names) funEnv'
    forM_ (fundefs `L.zip` logs) $ \((nm, (typ, annot)),log0) -> do
      prettyOut (SourceRepl.hasType      . sourceState) ("- Type: " <> show (pretty nm))      typ
      prettyOut (SourceRepl.hasAnnotated . sourceState) ("- Annotated: " <> show (pretty nm)) (Source.PrettyAnnot annot)
      lift $ when (SourceRepl.hasTypeCheckLog $ sourceState state) $ forM_ log0 $ \l -> do
        Repl.prettyHL l
        Repl.nl

    return $ state { sourceState = st { SourceRepl.dictionary = d { dictionaryFunctions = fromFunEnv funEnv' } } }

  -- We use the simulator to evaluate the Icicle expression.
  Nothing -> withError $ do
      let iid = [inputid|repl:input|]
      parsed       <- hoist $ sourceParse (T.pack line)
      (annot, typ) <- hoist $ sourceCheck checkOpts (SourceRepl.dictionary st) parsed

      prettyOut (SourceRepl.hasType      . sourceState) "- Type:"       typ
      prettyOut (SourceRepl.hasAnnotated . sourceState) "- Annotated:" (Source.PrettyAnnot annot)

      let inlined  = Source.sourceInline (SourceRepl.inlineOpt st) (SourceRepl.dictionary st) annot
      blanded     <- hoist $ sourceDesugar inlined

      prettyOut (SourceRepl.hasInlined . sourceState) "- Inlined:" inlined
      prettyOut (SourceRepl.hasDesugar . sourceState) "- Desugar:" blanded

      (annobland, _) <- hoist $ sourceCheck checkOpts (SourceRepl.dictionary st) blanded

      prettyOut (SourceRepl.hasDesugar . sourceState) "- Annotated desugar:" (Source.PrettyAnnot annobland)

      let reified = sourceReify annobland

      prettyOut (SourceRepl.hasReified . sourceState) "- Reified:"            reified
      prettyOut (SourceRepl.hasReified . sourceState) "- Reified annotated:" (Source.PrettyAnnot reified)

      let finalSource   = reified

      -- Core, simplified and unsimplified.

      coreUnsimped <- hoist $ sourceConvert (SourceRepl.dictionary st) finalSource

      let core  | doCoreSimp state = Compiler.coreSimp coreUnsimped
                | otherwise        = coreUnsimped

      if doCoreSimp state
      then prettyOut hasCore "- Core (simplified):" core
      else prettyOut hasCore "- Core (not simplified):" core

      case Core.checkProgram core of
       Left  e -> prettyOut (const True) "- Core type error:" e
       Right t -> prettyOut hasCoreType "- Core type:" t


      -- Avalanche, simplified.

      let avalancheSimped = Compiler.coreAvalanche core
      prettyOut hasAvalanche "- Avalanche (simplified):" avalancheSimped

      -- Flatten Avalanche, not simplified.

      let  avalancheFlatUnsimped = Compiler.flattenAvalanche avalancheSimped
      case avalancheFlatUnsimped of
       Left  e -> prettyOut (const True) "- Flatten Avalanche (not simplified) error:" e
       Right f -> do

      -- Flattened Avalanche, not simplified, check.

         let  flatUnsimpedChecked = Compiler.checkAvalanche $ Avalanche.eraseAnnotP f
         case flatUnsimpedChecked of
          Left  e  -> prettyOut (const True) "- Flattened Avalanche (not simplified) type error:" e
          Right f' -> prettyOut hasFlattenNoSimp "- Flattened Avalanche (not simplified), typechecked:" f'

      -- Flattened Avalanche, simplified.

      let  avalancheFlatSimped
              | doFlattenSimpCheck state
              = coreFlatten_ (Avalanche.SimpOpts True True) core
              | otherwise
              = coreFlatten core
      case avalancheFlatSimped of
       Left  e          -> prettyOut (const True) "- Flatten Avalanche (simplified) error:" e
       Right flatSimped -> do

        prettyOut hasFlattenSimp "- Flattened (simplified), not typechecked:" flatSimped

      -- Flattened Avalanche, simplified, check.

        let  flatChecked = checkAvalanche flatSimped
        case flatChecked of
         Left  e  -> prettyOut (const True) "- Flattened Avalanche (simplified) type error:" e
         Right f' -> do
           prettyOut hasFlattenSimp "- Flattened Avalanche (simplified), typechecked:" f'

      -- Flattened Avalanche, simplified, eval.
           let flatList = f' :| []

           case Compiler.avalancheEval (SourceRepl.stateEvalContext st) (SourceRepl.facts st) finalSource flatSimped of
            Left  e -> prettyOut hasAvalancheEval "- Flattened Avalanche (simplified) evalutation error:" e
            Right r -> prettyOut hasAvalancheEval "- Flattened Avalanche (simplified) evaluation:" r

      -- Sea

           prettyOut hasSeaPreamble "- C preamble:" Sea.seaPreamble

           when (hasSea state) $ do
             let seaProgram = Sea.seaOfPrograms 0 iid flatList
             case seaProgram of
               Left  e -> prettyOut (const True) "- C error:" e
               Right r -> prettyOut (const True) "- C:" r

           when (hasSeaAssembly state) $ do
             result <- liftIO . runEitherT $ Sea.assemblyOfPrograms "icicle-repl" Sea.NoInput [iid] [(iid, flatList)]
             case result of
               Left  e -> prettyOut (const True) "- C assembly error:" e
               Right r -> prettyOut (const True) "- C assembly:" r

           when (hasSeaLLVMIR state) $ do
             result <- liftIO . runEitherT $ Sea.irOfPrograms "icicle-repl" Sea.NoInput [iid] [(iid, flatList)]
             case result of
               Left  e -> prettyOut (const True) "- C LLVM IR error:" e
               Right r -> prettyOut (const True) "- C LLVM IR:" r

           when (hasSeaEval state) $ do
             result <- liftIO . runEitherT $ Compiler.seaEval (SourceRepl.stateEvalContext st) (SourceRepl.facts st) finalSource f'
             case result of
               Left  e -> prettyOut (const True) "- C error:" e
               Right r -> prettyOut (const True) "- C evaluation:" r

      case Compiler.coreEval (SourceRepl.stateEvalContext st) (SourceRepl.facts st) finalSource core of
       Left  e -> prettyOut hasCoreEval "- Core error:" e
       Right r -> prettyOut hasCoreEval "- Core evaluation:" r

      return state

  where

    hoist c = hoistEither c
    prettyOut setting heading p
            = lift
            $ when (setting state)
            $ do    HL.outputStrLn heading
                    Repl.prettyHL p
                    Repl.nl

    withError f = do
     c <- runEitherT f
     case c of
       Left  e -> do
        Repl.renderReplError e posOfError
        return state
       Right state' -> return state'


    checkOpts
      | SourceRepl.hasBigData (sourceState state)
      = Source.optionBigData
      | otherwise
      = Source.optionSmallData

    wrapSourceError = first (ErrorCompileSource . SourceRepl.ErrorCompile)

    sourceParse
      = first (ErrorCompileSource . SourceRepl.ErrorCompile)
      . Source.sourceParseQT [outputid|repl:output|]

    sourceDesugar
      = wrapSourceError
      . Source.sourceDesugarQT

    sourceReify
      = Source.sourceReifyQT

    sourceCheck opts d
      = wrapSourceError
      . Source.sourceCheckQT opts d

    sourceConvert d
     = first ErrorCompileCore . Compiler.sourceConvert d

    coreFlatten = first ErrorCompileAvalanche . Compiler.coreFlatten

    coreFlatten_ opts
     = first ErrorCompileAvalanche . Compiler.coreFlatten_ opts

    checkAvalanche
     = first ErrorCompileAvalanche . Compiler.checkAvalanche


--------------------------------------------------------------------------------

data ErrorRepl
 = ErrorCompileSource    (SourceRepl.ErrorSource)
 | ErrorCompileCore      (Compiler.ErrorCompile     Source.Var)
 | ErrorCompileAvalanche (Compiler.ErrorCompile     Source.Var)
 | ErrorRuntime          (Sim.SimulateError ()      Source.Var)

posOfError :: ErrorRepl -> Maybe Parsec.SourcePos
posOfError e
 = case e of
    ErrorCompileSource d
     -> SourceRepl.posOfError d
    ErrorCompileCore d
     -> Compiler.annotOfError d
    ErrorCompileAvalanche _
     -> Nothing
    ErrorRuntime _
     -> Nothing

instance Pretty ErrorRepl where
 pretty e
  = case e of
     ErrorCompileSource d
      -> pretty d
     ErrorCompileCore d
      -> pretty d
     ErrorCompileAvalanche d
      -> pretty d
     ErrorRuntime d
      -> "Runtime error:" <> Pretty.line
      <> Pretty.indent 2 (pretty d)

data DictionaryLoadType
 = DictionaryLoadTextV1 FilePath
 | DictionaryLoadToml   FilePath
 deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

showState :: ReplState -> HL.InputT IO ()
showState state = let st = sourceState state in do
 mapM_ HL.outputStrLn
    [ flag "type:            " (SourceRepl.hasType      . sourceState)
    , flag "type-check-log:  " (SourceRepl.hasTypeCheckLog . sourceState)
    , flag "big-data:        " (SourceRepl.hasBigData   . sourceState)
    , flag "annotated:       " (SourceRepl.hasAnnotated . sourceState)
    , flag "inlined:         " (SourceRepl.hasInlined   . sourceState)
    , flag "desugar:         " (SourceRepl.hasDesugar   . sourceState)
    , flag "reified:         " (SourceRepl.hasReified   . sourceState)
    , flag "core:            " hasCore
    , flag "core-type:       " hasCoreType
    , flag "core-simp:       " doCoreSimp
    , flag "core-eval:       " hasCoreEval
    , flag "avalanche:       " hasAvalanche
    , flag "flatten-no-simp: " hasFlattenNoSimp
    , flag "flatten-simp:    " hasFlattenSimp
    , flag "c-preamble:      " hasSeaPreamble
    , flag "c:               " hasSea
    , flag "c-assembly:      " hasSeaAssembly
    , flag "c-llvm-ir:       " hasSeaLLVMIR
    , flag "c-eval:          " hasSeaEval
    ,      "now:             " <> T.unpack (renderTime $ SourceRepl.currentTime st)
    ,      "max-map-size:    " <> show (SourceRepl.maxMapSize st)
    ,      "data:            " <> show (length $ SourceRepl.facts st) <> " rows"
    ]
 Repl.prettyHL $ prettyDictionarySummary (SourceRepl.dictionary st)
 where
  flag nm setting
   = nm <> Repl.showFlag (setting state)


usage :: HL.InputT IO ()
usage
 = mapM_ HL.outputStrLn
      [ "Usage:"
      , ":help or :h           -- shows this message"
      , ":quit or :q           -- quits the REPL"
      , ":load <filepath>      -- loads a data set"
      , ":dictionary <path>    -- loads a dictionary"
      , ":import <filepath>    -- imports functions from a file"
      , ":set  +/-type         -- whether to show the checked expression type"
      , ":set  +/-big-data     -- ... perform the big data check (only windows & latests allowed)"
      , ":set  +/-annotated    -- ... show the Source with inferred types as annotations"
      , ":set  +/-inlined      -- ... show the Source after inlining functions"
      , ":set  +/-desugar      -- ... show the Source after desugaring case expressions"
      , ":set  +/-reified      -- ... show the Source after reifying possibilities"
      , ":set  +/-core         -- ... show the Core conversion"
      , ":set  +/-core-type    -- ... show the Core conversion's type"
      , ":set  +/-core-simp    -- ... simplify the result of Core conversion"
      , ":set  +/-core-eval    -- ... show the result (using Core evaluation)"
      , ":set  +/-avalanche    -- ... show the Avalanche conversion"
      , ":set  +/-flatten      -- ... show flattened Avalanche conversion"
      , ":set  +/-c-preamble   -- ... show the C preamble"
      , ":set  +/-c            -- ... show the C conversion"
      , ":set  +/-c-assembly   -- ... show the C assembly"
      , ":set  +/-c-eval       -- ... show the result (using C evaluation)" ]


handleSetCommand :: ReplState -> Set -> HL.InputT IO ReplState
handleSetCommand state set = let st = sourceState state in
  case set of
    InlineOpt inl -> do
        let inline = case inl of
                       Source.InlineUsingLets  -> "lets"
                       Source.InlineUsingSubst -> "subst"
        HL.outputStrLn $ "ok, inline is now using " <> inline
        return $ state { sourceState = st { SourceRepl.inlineOpt = inl } }

    ShowType b -> do
        HL.outputStrLn $ "ok, type is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasType = b } }

    ShowTypeCheckLog b -> do
        HL.outputStrLn $ "ok, type-check-log is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasTypeCheckLog = b } }

    ShowBigData b -> do
        HL.outputStrLn $ "ok, big-data is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasBigData = b } }

    ShowAnnotated b -> do
        HL.outputStrLn $ "ok, annotated is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasAnnotated = b } }

    ShowInlined b -> do
        HL.outputStrLn $ "ok, inlined is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasInlined = b } }

    ShowDesugar b -> do
        HL.outputStrLn $ "ok, desugar is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasDesugar = b } }

    ShowReified b -> do
        HL.outputStrLn $ "ok, reified is now " <> Repl.showFlag b
        return $ state { sourceState = st { SourceRepl.hasReified = b } }

    ShowCore b -> do
        HL.outputStrLn $ "ok, core is now " <> Repl.showFlag b
        return $ state { hasCore = b }

    ShowCoreType b -> do
        HL.outputStrLn $ "ok, core-type is now " <> Repl.showFlag b
        return $ state { hasCoreType = b }

    ShowCoreEval b -> do
        HL.outputStrLn $ "ok, core evaluation is now " <> Repl.showFlag b
        return $ state { hasCoreEval = b }

    ShowAvalanche b -> do
        HL.outputStrLn $ "ok, avalanche is now " <> Repl.showFlag b
        return $ state { hasAvalanche = b }

    ShowAvalancheEval b -> do
        HL.outputStrLn $ "ok, avalanche eval is now " <> Repl.showFlag b
        return $ state { hasAvalancheEval = b }

    ShowFlattenNoSimp b -> do
        HL.outputStrLn $ "ok, flatten (not simplified) is now " <> Repl.showFlag b
        return $ state { hasFlattenNoSimp = b }

    ShowFlattenSimp b -> do
        HL.outputStrLn $ "ok, flatten (simplified) is now " <> Repl.showFlag b
        return $ state { hasFlattenSimp = b }

    ShowSeaPreamble b -> do
        HL.outputStrLn $ "ok, c preamble is now " <> Repl.showFlag b
        return $ state { hasSeaPreamble = b }

    ShowSea b -> do
        HL.outputStrLn $ "ok, c is now " <> Repl.showFlag b
        return $ state { hasSea = b }

    ShowSeaAssembly b -> do
        HL.outputStrLn $ "ok, c assembly is now " <> Repl.showFlag b
        return $ state { hasSeaAssembly = b }

    ShowSeaLLVMIR b -> do
        HL.outputStrLn $ "ok, c llvm ir is now " <> Repl.showFlag b
        return $ state { hasSeaLLVMIR = b }

    ShowSeaEval b -> do
        HL.outputStrLn $ "ok, c evaluation now " <> Repl.showFlag b
        when b $ do
          HL.outputStrLn "                   _________-----_____"
          HL.outputStrLn "        _____------           __      ----_"
          HL.outputStrLn " ___----             ___------              \\"
          HL.outputStrLn "    ----________        ----                 \\"
          HL.outputStrLn "                -----__    |             _____)"
          HL.outputStrLn "                     __-                /     \\"
          HL.outputStrLn "         _______-----    ___--          \\    /)\\"
          HL.outputStrLn "   ------_______      ---____            \\__/  /"
          HL.outputStrLn "                -----__    \\ --    _          /\\"
          HL.outputStrLn "                       --__--__     \\_____/   \\_/\\"
          HL.outputStrLn "                               ----|   /          |"
          HL.outputStrLn "                                   |  |___________|"
          HL.outputStrLn "                                   |  | ((_(_)| )_)"
          HL.outputStrLn "                                   |  \\_((_(_)|/(_)"
          HL.outputStrLn "                                   \\             ("
          HL.outputStrLn "                                    \\_____________)"
        return $ state { hasSeaEval = b }

    CurrentTime d -> do
        HL.outputStrLn $ "ok, time set to " <> T.unpack (renderTime d)
        return $ state { sourceState = st { SourceRepl.currentTime = d } }

    SetMaxMapSize m -> do
        HL.outputStrLn $ "ok, max-map-size set to " <> show m
        return $ state { sourceState = st { SourceRepl.maxMapSize = m } }

    PerformCoreSimp b -> do
        HL.outputStrLn $ "ok, core-simp is now " <> Repl.showFlag b
        return $ state { doCoreSimp = b }

    PerformFlattenSimpCheck b -> do
        HL.outputStrLn $ "ok, flatten-simp-check is now " <> Repl.showFlag b
        return $ state { doFlattenSimpCheck = b }
