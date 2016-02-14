{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.Psv.Output where

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Avalanche.Prim.Flat (Prim(..), PrimUnsafe(..))
import           Icicle.Avalanche.Prim.Flat (meltType)

import           Icicle.Common.Base (OutputName(..))
import           Icicle.Common.Type (ValType(..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base (seaOfAttributeDesc)
import           Icicle.Sea.FromAvalanche.Base (seaOfNameIx, seaOfChar)
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.Psv.Base

import           P
import           Prelude (String)


data PsvOutputConfig = PsvOutputConfig {
    outputPsvMode   :: PsvMode
  , outputPsvFormat :: PsvOutputFormat
  } deriving (Eq, Ord, Show)

data PsvOutputFormat
  = PsvOutputSparse
  | PsvOutputDense MissingValue
  deriving (Eq, Ord, Show)

type MissingValue = Text

defaultMissingValue :: Text
defaultMissingValue = "NA"

------------------------------------------------------------------------

seaOfWriteFleetOutput :: PsvOutputConfig -> [SeaProgramState] -> Either SeaError Doc
seaOfWriteFleetOutput config states = do
  write_sea <- traverse (seaOfWriteProgramOutput config) states
  let (beforeChord, inChord, afterChord)
         = case outputPsvFormat config of
             PsvOutputDense _
              -> (outputEntity, outputChord, outputChar '\n')
             PsvOutputSparse
              -> ("", "", "")

  pure $ vsep
    [ "#line 1 \"write all outputs\""
    , "static ierror_msg_t psv_write_outputs"
    , "    ( int fd"
    , "    , char  *buffer"
    , "    , char  *buffer_end"
    , "    , char **buffer_ptr_ptr"
    , "    , const char *entity"
    , "    , size_t entity_size"
    , "    , ifleet_t *fleet )"
    , "{"
    , "    iint_t         chord_count = fleet->chord_count;"
    , "    const itime_t *chord_times = fleet->chord_times;"
    , "    ierror_msg_t   error;"
    , ""
    , "    char *buffer_ptr = *buffer_ptr_ptr;"
    , ""
    , indent 4 beforeChord
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , indent 8 (seaOfChordTime $ outputPsvMode config)
    , indent 8 (vsep write_sea)
    , indent 8 inChord
    , "    }"
    , indent 4 afterChord
    , ""
    , "    *buffer_ptr_ptr = buffer_ptr;"
    , ""
    , "    return 0;"
    , "}"
    ]

seaOfChordTime :: PsvMode -> Doc
seaOfChordTime = \case
  PsvSnapshot _ -> vsep
    [ "const char  *chord_time = \"\";"
    , "const size_t chord_size = 0;"
    ]
  PsvChords     -> vsep
    [ "iint_t c_year, c_month, c_day, c_hour, c_minute, c_second;"
    , "itime_to_gregorian (chord_times[chord_ix], &c_year, &c_month, &c_day, &c_hour, &c_minute, &c_second);"
    , ""
    , "const size_t chord_size = sizeof (\"|yyyy-mm-ddThh:mm:ssZ\") - 1;"
    , "char chord_time[chord_size + 1];"
    , "snprintf (chord_time, chord_size + 1, \"|" <> timeFmt <> "Z\", "
             <> "c_year, c_month, c_day, c_hour, c_minute, c_second);"
    ]

outputChord :: Doc
outputChord
  = outputValue "string" ["chord_time", "chord_size"]


seaOfWriteProgramOutput :: PsvOutputConfig -> SeaProgramState -> Either SeaError Doc
seaOfWriteProgramOutput config state = do
  let ps    = "p" <> int (stateName state)
      stype = pretty (nameOfStateType state)
      pname = pretty (nameOfProgram state)

  let outputState (name, (ty, tys))
        = case outputPsvFormat config of
            PsvOutputSparse
              -> seaOfWriteOutputSparse ps 0 name ty tys
            PsvOutputDense missingValue
              -> seaOfWriteOutputDense  ps 0 name ty tys missingValue

  let outputRes   name
        = ps <> "->" <> pretty (hasPrefix <> name) <+> "= ifalse;"

  let resumeables  = fmap (outputRes . fst) (stateResumables state)
  outputs         <- traverse outputState (stateOutputs state)

  pure $ vsep
    [ ""
    , "/* " <> seaOfAttributeDesc (stateAttribute state) <> " */"
    , stype <+> "*" <> ps <+> "=" <+> "&fleet->" <> pname <> "[chord_ix];"
    , pname <+> "(" <> ps <> ");"
    , ps <> "->new_count = 0;"
    , vsep resumeables
    , ""
    , vsep outputs
    ]

seaOfWriteOutputSparse :: Doc -> Int -> OutputName -> ValType -> [ValType] -> Either SeaError Doc
seaOfWriteOutputSparse struct structIndex outName@(OutputName name) outType argTypes
  = let members = structMembers struct name argTypes structIndex
    in case outType of
         -- Top-level Sum is a special case, to avoid allocating and printing if
         -- the whole computation is an error (e.g. tombstone)
         SumT ErrorT outType'
          | (ErrorT : argTypes') <- argTypes
          , (ne     : _)         <- members
          -> do (m, body, _, _) <- seaOfOutput NotInJSON struct (structIndex + 1)
                                               outName Map.empty outType' argTypes' id
                let body'        = seaOfOutputCond m body
                let body''       = go body'
                pure $ conditionalNotError' ne body''
         _
          -> do (m, body, _, _) <- seaOfOutput NotInJSON struct structIndex
                                               outName Map.empty outType argTypes id
                let body'        = seaOfOutputCond m body
                return $ go body'

  where
    go str = vsep [ outputEntity
                  , outputChar '|'
                  , outputAttr name
                  , outputChar '|'
                  , str
                  , outputChord
                  , outputChar '\n'
                  ]

seaOfWriteOutputDense :: Doc -> Int -> OutputName -> ValType -> [ValType] -> MissingValue -> Either SeaError Doc
seaOfWriteOutputDense struct structIndex outName@(OutputName name) outType argTypes missingValue
  = let members = structMembers struct name argTypes structIndex
    in  case outType of
         SumT ErrorT outType'
          | (ErrorT : argTypes') <- argTypes
          , (ne     : _)         <- members
          -> do (m, body, _, _) <- seaOfOutput NotInJSON struct (structIndex + 1)
                                               outName Map.empty outType' argTypes' id
                let body'        = seaOfOutputCond m body
                go $ conditionalNotError ne body' bodyMissingValue
         _
          -> do (m, body, _, _) <- seaOfOutput NotInJSON struct structIndex
                                               outName Map.empty outType argTypes id
                let body'        = seaOfOutputCond m body
                go body'
  where
    -- Missing value needs to be unquoted
    bodyMissingValue
      = outputValue "string" ["\"" <> pretty missingValue <> "\"", pretty (T.length missingValue)]

    go body
      = pure
      $ vsep [ outputChar '|', body ]

-- | Construct the struct member names for the output arguments.
--
structMembers :: Doc -> Text -> [ValType] -> Int -> [Doc]
structMembers struct name argTypes structIndex
  = List.take (length argTypes)
  $ fmap (\ix -> struct <> "->" <> seaOfNameIx name ix)
         [structIndex..]

-- | Output the entity, e.g "homer|"
--
outputEntity :: Doc
outputEntity
  = outputValue  "string" ["entity", "entity_size"]

-- | Output the attribute, e.g "salary|"
--
outputAttr :: Text -> Doc
outputAttr = outputString


--------------------------------------------------------------------------------

-- | A mapping of C name prefixes in use to the number of times they are used.
--   e.g. if @x_0@ and @x_1@ have been used, the environment must contain @(x, 2)@
--
type NameEnv = Map String Int

newName :: Doc -> NameEnv -> (Doc, NameEnv)
newName doc env
  = let n = show doc
    in  case Map.lookup n env of
          Nothing
            -> (pretty (0 :: Int), Map.insert n 1 env)
          Just i
            -> (pretty i, Map.insert n (i + 1) env)

seaOfOutput
  :: IsInJSON                      -- ^ Indicates whether to quote strings
  -> Doc                           -- ^ Struct of values to output
  -> Int                           -- ^ Current index into the struct of values
  -> OutputName                    -- ^ Use the output name as seed for generated C names
  -> NameEnv                       -- ^ C names in use
  -> ValType                       -- ^ Output type
  -> [ValType]                     -- ^ Types of arguments
  -> (Doc -> Doc)                  -- ^ Transformation to be applied to this struct member, e.g. index
  -> Either SeaError ( Maybe Doc   -- Top level condition for the output statement
                     , Doc         -- The output statement
                     , Int         -- Where it's up to
                     , [ValType] ) -- Unconsumed arguments
seaOfOutput isJSON struct structIndex outName@(OutputName name) env outType argTypes transform
 = let prefixi         = pretty name <> "_" <> pretty structIndex <> "_i"
       (suffixi, env'')= newName prefixi env
       counter         = prefixi <> suffixi

       prefixn         = pretty name <> "_" <> pretty structIndex <> "_n"
       (suffixn, env') = newName prefixn env''
       countLimit      = prefixn <> suffixn

       arrayIndex t x  = seaOfArrayIndex x counter t

   in case outType of
       ArrayT te
        | tes@(arg0:_) <- meltType te
        , (arr  : _)   <- members
        -> do (mcond, body, ix, ts1) <- seaOfOutput InJSON struct structIndex
                                                    outName env' te tes
                                                    (arrayIndex arg0 . transform)

              -- End the body with a comma, if applicable
              let body' = seaOfOutputCond mcond
                        $ vsep [seaOfOutputArraySep counter, body]

              -- Array of arrays is allowed, so we apply the transform here
              let arr'  = transform arr

              -- Wrap the body in a for loop
              let numElems  = arrayCount arr'
              body''       <- seaOfOutputArray body' numElems counter countLimit

              return (Nothing, body'', ix, ts1)


       MapT tk tv
        | tks@(argk0 : _) <- meltType tk
        , tvs@(argv0 : _) <- meltType tv
        , (arr : _)       <- members
        -> do (mcondk, bk, ixk, _)  <- seaOfOutput InJSON struct structIndex
                                                   outName env' tk tks
                                                   (arrayIndex argk0 . transform)
              (mcondv, bv, ixv, ts) <- seaOfOutput InJSON struct ixk
                                                   outName env' tv tvs
                                                   (arrayIndex argv0 . transform)

              let p  = pair bk bv
              let p' = seaOfOutputCond mcondk
                     $ seaOfOutputCond mcondv
                     $ vsep [seaOfOutputArraySep counter, p]

              let numElems  = arrayCount arr
              body         <- seaOfOutputArray p' numElems counter countLimit
              return (Nothing, body, ixv, ts)


       PairT ta tb
        | tas <- meltType ta
        , tbs <- meltType tb
        -> do (mcondk, ba, ixa, _)  <- seaOfOutput InJSON struct structIndex
                                                   outName env' ta tas transform
              (mcondv, bb, ixb, ts) <- seaOfOutput InJSON struct ixa
                                                   outName env' tb tbs transform

              let p  = pair ba bb
              let p' = seaOfOutputCond mcondk
                     $ seaOfOutputCond mcondv
                     $ p

              return (Nothing, p', ixb, ts)

       -- Conditional's
       OptionT otype1
        | (BoolT : ts1) <- argTypes
        , (nb    : _)   <- members
        -> do (mcond, body, ix, ts) <- seaOfOutput isJSON struct (structIndex + 1)
                                                   outName env' otype1 ts1 transform

              let body' = seaOfOutputCond mcond body
              let nb'   = transform nb
              pure (Just nb', body', ix, ts)

       SumT ErrorT otype1
        | (ErrorT : ts1) <- argTypes
        , (ne     : _)   <- members
        -> do (mcond, body, ix, ts) <- seaOfOutput isJSON struct (structIndex + 1)
                                                   outName env' otype1 ts1 transform

              let body' = seaOfOutputCond mcond body
              let ne'   = transform ne
              pure (Just (ne' <> " == ierror_not_an_error"), body', ix, ts)

       -- Base
       _
        | (t  : ts) <- argTypes
        , (mx : _)  <- members
        , mx'       <- transform mx
        -> do d <- seaOfOutputBase' isJSON t mx'
              pure (Nothing, d, structIndex + 1, ts)

       _ -> Left unsupported

  where
   mismatch    = SeaOutputTypeMismatch    outName outType argTypes
   unsupported = SeaUnsupportedOutputType outType

   members    = List.take (length argTypes)
              $ fmap (\ix -> struct <> "->" <> seaOfNameIx name ix) [structIndex..]

   arrayCount x
    = "(" <> x <> ")" <> "->count"

   seaOfOutputBase' b
    = seaOfOutputBase b mismatch

   seaOfOutputArraySep c
     = conditional' (c <+> "> 0") seaOfOutputSep

   seaOfOutputSep
     = outputChar ','

--------------------------------------------------------------------------------

seaOfArrayIndex :: Doc -> Doc -> ValType -> Doc
seaOfArrayIndex arr ix typ
 = seaOfPrimDocApps (seaOfXPrim (PrimUnsafe (PrimUnsafeArrayIndex typ)))
                    [ arr, ix ]

-- | Output an array with pre-defined bodies
seaOfOutputArray :: Applicative f => Doc -> Doc -> Doc -> Doc -> f Doc
seaOfOutputArray body numElems counter countLimit
 = pure (vsep [ outputChar '['
              , forStmt counter countLimit numElems
              , "{"
              , indent 4 body
              , "}"
              , outputChar ']'
              ]
        )

-- | Output an if statement
seaOfOutputCond :: Maybe Doc -> Doc -> Doc
seaOfOutputCond mcond body
  = case mcond of
      Nothing
        -> body
      Just cond
        -> conditional' cond body

-- | Output single types
seaOfOutputBase :: IsInJSON -> SeaError -> ValType -> Doc -> Either SeaError Doc
seaOfOutputBase quoteStrings err t val
 = case t of
     BoolT
      -> pure
       $ vsep
           [ "if (" <> val <> ") {"
           , indent 4 $ outputString "true"
           , "} else {"
           , indent 4 $ outputString "false"
           , "}"
           ]
     IntT
      -> pure $ outputValue "int" [val]
     DoubleT
      -> pure $ outputValue "double" [val]
     StringT
      -> pure $ quotedOutput quoteStrings (outputValue "string" [val, "strlen(" <> val <> ")"])
     TimeT
      -> pure $ quotedOutput quoteStrings (outputValue "time" [val])

     _ -> Left err

------------------------------------------------------------------------

-- | A hack to tell whether or not strings should be quoted.
--   If in JSON, quote. If not, don't.
--   At any stage during output, if the elements should be JSON, pass @InJSON@,
--   e.g. when outputting arrays, we should specify that the elements be output
--        as JSON.
--
data IsInJSON = InJSON | NotInJSON

conditional' :: Doc -> Doc -> Doc
conditional' n body
 = vsep ["if (" <> n <> ")"
        , "{"
        , indent 4 body
        , "}"]

conditional :: Doc -> Doc -> Doc -> Doc
conditional n body1 body2
 = vsep ["if (" <> n <> ")"
        , "{"
        , indent 4 body1
        , "} else {"
        , indent 4 body2
        , "}"]

conditionalNotError' :: Doc -> Doc -> Doc
conditionalNotError' n body
 = conditional' (n <> " == ierror_not_an_error") body

conditionalNotError :: Doc -> Doc -> Doc -> Doc
conditionalNotError n body1 body2
 = conditional (n <> " == ierror_not_an_error") body1 body2

pair :: Doc -> Doc -> Doc
pair x y
 = vsep [ outputChar '['
        , x
        , outputChar ','
        , y
        , outputChar ']'
        ]

outputValue :: Doc -> [Doc] -> Doc
outputValue typ vals
 = vsep
 [ "error = psv_output_" <> typ <> " "
   <> "(fd, buffer, buffer_end, &buffer_ptr, " <> val <> ");"
 , outputDie
 ]
 where
  val = hcat (punctuate ", " vals)

forStmt :: Doc -> Doc -> Doc -> Doc
forStmt i n m
 = "for(iint_t" <+> i <+> "= 0," <+> n <+> "=" <+> m <> ";" <+> i <+> "<" <+> n <> "; ++" <> i <> ")"

outputChar :: Char -> Doc
outputChar x
 = outputValue "char" [seaOfChar x]

outputString :: Text -> Doc
outputString xs
 = vsep
 [ "if (buffer_end - buffer_ptr < " <> int rounded <> ") {"
 , "    error = psv_output_flush (fd, buffer, &buffer_ptr);"
 , indent 4 outputDie
 , "}"
 , vsep (fmap mkdoc swords)
 , "buffer_ptr += " <> int size <> ";"
 ]
 where
  swords = wordsOfString xs

  rounded  = length swords * 8
  size     = sum (fmap swSize swords)
  mkdoc sw = "*(uint64_t *)(buffer_ptr + " <> int (swOffset sw) <> ") = " <> swBits sw <> ";"

timeFmt :: Doc
timeFmt = "%04lld-%02lld-%02lldT%02lld:%02lld:%02lld"

outputDie :: Doc
outputDie = "if (error) return error;"

quotedOutput :: IsInJSON -> Doc -> Doc
quotedOutput NotInJSON out = out
quotedOutput InJSON    out = vsep [outputChar '"', out, outputChar '"']
