{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.IO.Psv.Output where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

import           Icicle.Avalanche.Prim.Flat (Prim(..), PrimUnsafe(..))
import           Icicle.Avalanche.Prim.Flat (meltType)

import           Icicle.Common.Type (ValType(..), StructType(..), StructField(..))

import           Icicle.Data.Name

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.Base hiding (assign)
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv.Schema
import           Icicle.Sea.Name

import           P
import           Prelude (String)


data PsvOutputConfig = PsvOutputConfig {
    outputPsvMode      :: Mode
  , outputPsvFormat    :: PsvOutputFormat
  , outputPsvMissing   :: Text
  } deriving (Eq, Ord, Show)

data PsvOutputFormat
  = PsvOutputSparse
  | PsvOutputDense
  deriving (Eq, Ord, Show)

type PsvOutputWhiteList = Maybe [Text]

data PsvMissing
  = PsvMissing Text
  | PsvDrop

defaultOutputMissing :: Text
defaultOutputMissing = "NA"

------------------------------------------------------------------------

seaOfWriteFleetOutput :: PsvOutputConfig -> PsvOutputWhiteList -> [Cluster c k] -> Either SeaError Doc
seaOfWriteFleetOutput config whitelist states = do
  let states' = case whitelist of
                  Nothing -> states
                  Just as -> filter (flip List.elem as . renderInputName . inputName . clusterInputId) states

  write_sea <- traverse (seaOfWriteProgramOutput config) states'
  schema_sea <- seaOfGetOutputSchema config states'

  let (beforeChord, inChord, afterChord)
         = case outputPsvFormat config of
             PsvOutputDense
              -> (outputEntity, outputChord, outputChar '\n')
             PsvOutputSparse
              -> ("", "", "")

  pure $ vsep
    [ "#line 1 \"write all outputs\""
    , "static ierror_msg_t psv_write_output"
    , "    ( int fd"
    , "    , char  *buffer"
    , "    , char  *buffer_end"
    , "    , char **buffer_ptr_ptr"
    , "    , const char *entity"
    , "    , size_t entity_size"
    , "    , ifleet_t *fleet )"
    , "{"
    , "    iint_t         chord_count = fleet->chord_count;"
    , "    const int64_t *chord_name_lengths = fleet->chord_name_lengths;"
    , "    const int64_t *chord_name_offsets = fleet->chord_name_offsets;"
    , "    const uint8_t *chord_name_data = fleet->chord_name_data;"
    , "    ierror_msg_t   error;"
    , ""
    , "    char *buffer_ptr = *buffer_ptr_ptr;"
    , ""
    , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
    , indent 8 beforeChord
    , indent 8 (seaOfChordName $ outputPsvMode config)
    , indent 8 (vsep write_sea)
    , indent 8 inChord
    , indent 8 afterChord
    , "    }"
    , ""
    , "    *buffer_ptr_ptr = buffer_ptr;"
    , ""
    , "    return 0;"
    , "}"
    , ""
    , schema_sea
    ]

seaOfChordName :: Mode -> Doc
seaOfChordName = \case
  Snapshot _ -> vsep
    [ "const char  *chord_name = NULL;"
    , "const size_t chord_name_length = 0;"
    ]
  Chords -> vsep
    [ "const int64_t  chord_name_offset = chord_name_offsets[chord_ix];"
    , "const int64_t  chord_name_length = chord_name_lengths[chord_ix];"
    , "const char    *chord_name = (char*) (chord_name_data + chord_name_offset);"
    ]

timeFmt :: Doc
timeFmt = "%04\" PRId64 \"-%02\" PRId64 \"-%02\" PRId64 \"T%02\" PRId64 \":%02\" PRId64 \":%02\" PRId64 \"Z"

outputChord :: Doc
outputChord
  = vsep
  [ "if (chord_name != NULL) {"
  , indent 4 $ outputChar '|'
  , indent 4 $ outputValue "string" ["chord_name", "chord_name_length"]
  , "}"
  ]

seaOfConstantString :: Text -> Doc
seaOfConstantString =
  pretty . show -- FIXME this could be more robust, but I think it's OK for schemas

textGroups80 :: Text -> [Text]
textGroups80 xs =
  let
    (hd, tl) =
      Text.splitAt 80 xs
  in
    if Text.null tl then
      [hd]
    else
      hd : textGroups80 tl

seaOfGetOutputSchema :: PsvOutputConfig -> [Cluster c k] -> Either SeaError Doc
seaOfGetOutputSchema config clusters =
  case outputPsvFormat config of
    PsvOutputSparse ->
      pure $ vsep [
          "istring_t psv_get_output_schema ()"
        , "{"
        , "    return NULL; /* no schema for sparse output */"
        , "}"
        ]

    PsvOutputDense -> do
      schema <- renderCompactPsvSchema <$> schemaOfFleet config clusters
      pure $ vsep [
          "istring_t psv_get_output_schema ()"
        , "{"
        , "    return"
        , indent 8 (vsep . fmap seaOfConstantString $ textGroups80 schema) <> ";"
        , "}"
        ]

seaOfWriteProgramOutput :: PsvOutputConfig -> Cluster c k -> Either SeaError Doc
seaOfWriteProgramOutput config cluster = do
  let ps    = "p" <> prettyClusterId (clusterId cluster)
      stype = pretty (nameOfClusterState cluster)
      attr  = pretty (nameOfCluster cluster)
      tb    = outputPsvMissing config

  let outputState (name, MeltedType ty tys)
        = case outputPsvFormat config of
            PsvOutputSparse
              -> seaOfWriteOutputSparse ps 0 name ty tys
            PsvOutputDense
              -> seaOfWriteOutputDense  ps 0 name ty tys tb

  let resStart compute = "(void*)(&" <> ps <> "->" <> nameOfResumableHasFlagsStart compute <> ")"
  let resEnd   compute = "(void*)(&" <> ps <> "->" <> nameOfResumableHasFlagsEnd   compute <> ")"
  let clearResumables compute
        = "memset (" <> resStart compute <> ", 0, " <> resEnd compute <> " - " <> resStart compute <> ");"

  let kernels = NonEmpty.toList $ clusterKernels cluster
  let resumeables  = fmap clearResumables kernels
  outputs         <- traverse outputState . Map.toList $ clusterOutputs cluster
  let callKernels = fmap (\i -> pretty (nameOfKernel i) <+> "(" <> ps <> ");") kernels

  pure $ vsep
    [ ""
    , "/* " <> prettyText (renderInputId $ clusterInputId cluster) <> " */"
    , stype <+> "*" <> ps <+> "=" <+> "&fleet->" <> attr <> "[chord_ix];"
    , vsep callKernels
    , ps <> "->input.new_count = 0;"
    , vsep resumeables
    , ""
    , vsep outputs
    ]

seaOfWriteOutputSparse :: Doc -> Int -> OutputId -> ValType -> [ValType] -> Either SeaError Doc
seaOfWriteOutputSparse struct structIndex outId outType argTypes
  = do (m, f, body, _, _)
          <- seaOfOutput NotInJSON struct structIndex outId PsvDrop Map.empty outType argTypes (const id)
       return $ seaOfOutputCond m f (go body)
  where
    go str = vsep [ outputEntity
                  , outputChar '|'
                  , outputAttr outId
                  , outputChar '|'
                  , str
                  , outputChord
                  , outputChar '\n'
                  ]

seaOfWriteOutputDense :: Doc -> Int -> OutputId -> ValType -> [ValType] -> Text -> Either SeaError Doc
seaOfWriteOutputDense struct structIndex outId outType argTypes missing
  = do (m, f, body, _, _)
          <- seaOfOutput NotInJSON struct structIndex outId (PsvMissing missing) Map.empty outType argTypes (const id)
       let body' = seaOfOutputCond m f body
       pure $ vsep [ outputChar '|', body' ]

-- | Output the entity, e.g "homer|"
--
outputEntity :: Doc
outputEntity
  = outputValue  "string" ["entity", "entity_size"]

-- | Output the attribute, e.g "employment:salary|"
--
outputAttr :: OutputId -> Doc
outputAttr = outputString . renderOutputId


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
  -> OutputId                      -- ^ Use the output name as seed for generated C names
  -> PsvMissing                    -- ^ Drop missing values or output something
  -> NameEnv                       -- ^ C names in use
  -> ValType                       -- ^ Output type
  -> [ValType]                     -- ^ Unmelted types of arguments
  -> (ValType -> Doc -> Doc)       -- ^ Transformation to be applied to this struct member, e.g. index
  -> Either SeaError ( Maybe Doc   -- Output the value when this is true
                     , Maybe Doc   -- Otherwise output this
                     , Doc         -- The output statement, x
                     , Int         -- Where it's up to
                     , [ValType] ) -- Unconsumed arguments
seaOfOutput isJSON struct structIndex outId missing env outType argTypes transform
 = let prefixi         = prettySeaName (mangleIx outId structIndex) <> "_i"
       (suffixi, env'')= newName prefixi env
       counter         = prefixi <> suffixi

       prefixn         = prettySeaName (mangleIx outId structIndex) <> "_n"
       (suffixn, env') = newName prefixn env''
       countLimit      = prefixn <> suffixn

       arrayIndex t x  = seaOfArrayIndex x counter t

       arrayIndex' t x = arrayIndex t $ transform (ArrayT t) x

   in case outType of
       ArrayT te
        | tes@(arg0 : _) <- meltType te
        , arr : _  <- members
        -> do (mcond, _, body, ix, tes_empty)
                 <- seaOfOutput InJSON struct structIndex outId PsvDrop env' te tes arrayIndex'

              unless (null tes_empty) (Left mismatch)

              -- For nested arrays, we get the inner array out first,
              -- so we can retrieve the correct count. For flat arrays the transform
              -- is just id.
              --
              -- Maps are similar (see below).
              --
              let arr'  = transform (ArrayT arg0) arr

              -- Wrap the body in a for loop
              let numElems  = arrayCount arr'
              body'        <- seaOfOutputArray mcond body numElems counter countLimit

              return (Nothing, Nothing, body', ix, drop (length tes) argTypes)


       MapT tk tv
        | tks@(arg0 : _) <- meltType tk
        , tvs <- meltType tv
        , arr : _ <- members
        -> do (mcondk, _, bk, ixk, tks_empty)
                 <- seaOfOutput InJSON struct structIndex outId PsvDrop env' tk tks arrayIndex'
              (mcondv, _, bv, ixv, tvs_empty)
                 <- seaOfOutput InJSON struct ixk outId PsvDrop env' tv tvs arrayIndex'

              unless (null tks_empty && null tvs_empty) (Left mismatch)

              let p         = pair bk bv
              let arr'      = transform (ArrayT arg0) arr
              let numElems  = arrayCount arr'

              body <- seaOfOutputArray (condAnd mcondk mcondv) p numElems counter countLimit

              return (Nothing, Nothing, body, ixv, drop (length tks + length tvs) argTypes)


       PairT ta tb
        -> do (mcondk, _, ba, ixa, ts0)
                 <- seaOfOutput InJSON struct structIndex outId PsvDrop env' ta argTypes transform
              (mcondv, _, bb, ixb, ts1)
                 <- seaOfOutput InJSON struct ixa outId PsvDrop env' tb ts0 transform

              let p  = pair ba bb
              let p' = seaOfOutputCond' (condAnd mcondk mcondv) $ p

              return (condAnd mcondk mcondv, outputMissing, p', ixb, ts1)


       StructT fs
        | fields <- Map.toList (getStructType fs)
        -> do let go (ix, ts, docs) (n, t) = do
                    (cond, _, body, ix', ts') <- seaOfOutput InJSON struct ix outId PsvDrop env' t ts transform

                    let doc = vsep
                            [ outputChar '\"'
                            , outputString (nameOfStructField n)
                            , outputChar '\"'
                            , outputChar ':'
                            , body
                            ]
                    pure (ix', ts', docs <> [(cond, doc)])

              (ix, ts, docs) <- foldM go (structIndex, argTypes, mempty) fields
              let doc         = vsep $ [ outputChar '{' , seaOfOutputStructSep docs , outputChar '}' ]
              return (Nothing, Nothing, doc, ix, ts)


       -- Conditional's
       OptionT otype1
        | BoolT : ts0 <- argTypes
        , nb    : _   <- members
        -> do (mcond, mfalse, body, ix, ts1)
                 <- seaOfOutput isJSON struct (structIndex + 1) outId missing env' otype1 ts0 transform

              let body' = seaOfOutputCond mcond mfalse body
              let nb'   = transform BoolT nb
              pure ( condAnd (Just nb') mcond
                   , outputMissing, body', ix, ts1 )

       SumT ErrorT otype1
        | ErrorT : ts0 <- argTypes
        , ne     : _   <- members
        -> do (mcond, mfalse, body, ix, ts1)
                 <- seaOfOutput isJSON struct (structIndex + 1) outId missing env' otype1 ts0 transform

              let ne'   = transform ErrorT ne
              let body' = seaOfOutputCond mcond mfalse body
              let condSum = condAnd (Just (seaOfNotAnError ne')) mcond
              pure ( condSum, outputMissing, body', ix, ts1 )

       BufT _ a ->
         seaOfOutput isJSON struct structIndex outId missing env (ArrayT a) argTypes transform

       -- Base
       typ
        | typ `elem` [BoolT, TimeT, DoubleT, IntT, StringT, FactIdentifierT]
        , (t  : ts) <- argTypes
        , (mx : _)  <- members
        , mx'       <- transform t mx
        -> do d <- seaOfOutputBase' isJSON t mx'
              pure (Nothing, Nothing, d, structIndex + 1, ts)

       _ ->
         Left mismatch

  where
   mismatch    = SeaOutputTypeMismatch    outId outType argTypes

   members    = List.take (length argTypes)
              $ fmap (\ix -> struct <> "->" <> prettySeaName (mangleIx outId ix)) [structIndex..]

   arrayCount x
     = "(" <> x <> ")" <> "->count"

   condAnd Nothing y
     = y
   condAnd x Nothing
     = x
   condAnd (Just x) (Just y)
     = Just ("(" <> x <> ") && (" <> y <> ")")

   outputMissing
     = case missing of
         PsvDrop
           -> Nothing
         PsvMissing s
           -> Just (outputString s)

   seaOfOutputBase' b
     = seaOfOutputBase b mismatch

seaOfNotAnError :: Doc -> Doc
seaOfNotAnError v =
  "(" <> v <> " == ierror_not_an_error)"

--------------------------------------------------------------------------------

seaOfArrayIndex :: Doc -> Doc -> ValType -> Doc
seaOfArrayIndex arr ix typ
 = seaOfPrimDocApps (seaOfXPrim (PrimUnsafe (PrimUnsafeArrayIndex typ)))
                    [ arr, ix ]

-- | Output an array with pre-defined bodies
seaOfOutputArray :: Applicative f => Maybe Doc -> Doc -> Doc -> Doc -> Doc -> f Doc
seaOfOutputArray mcond body numElems counter countLimit
  = let needSep
          = counter <> "_sep"

        go (Just cond)
          = conditional' cond (withSep (needSep <> " = " <> cond <> ";"))
        go Nothing
          = withSep (needSep <> " = itrue;")

        withSep assign
          = vsep
          [ conditional' needSep (outputChar ',')
          , body
          , assign ]

    in pure (vsep [
                outputChar '['
              , "{"
              , "ibool_t " <> needSep <> " = ifalse;"
              , forStmt counter countLimit numElems
              , "{"
              , indent 4 (go mcond)
              , "}"
              , outputChar ']'
              , "}"
              ])

seaOfOutputStructSep :: [(Maybe Doc, Doc)] -> Doc
seaOfOutputStructSep fs
  = let go (Just cond, body)
          = conditional' cond
             (withStructSep body  ("need_struct_sep = " <> cond <> ";"))
        go (Nothing, body)
          = withStructSep body "need_struct_sep = itrue;"

        withStructSep body assign
          = vsep
          [ conditional' "need_struct_sep" (outputChar ',')
          , body
          , assign ]

    in  vsep [ "{", "ibool_t need_struct_sep = ifalse;", vsep $ fmap go fs, "}" ]

-- | Output an if statement
seaOfOutputCond :: Maybe Doc -> Maybe Doc -> Doc -> Doc
seaOfOutputCond mcond if_false if_true
  = case mcond of
      Nothing
        -> if_true
      Just cond
        -> case if_false of
             Nothing
               -> conditional' cond if_true
             Just x
               -> conditional cond if_true x

seaOfOutputCond' :: Maybe Doc -> Doc -> Doc
seaOfOutputCond' mcond if_true
  = case mcond of
      Nothing
        -> if_true
      Just cond
        -> conditional' cond if_true

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
      -> pure $ quotedOutput quoteStrings (outputValue "string" [val, "istring_size(" <> val <> ")"])
     TimeT
      -> pure $ quotedOutput quoteStrings (outputValue "time" [val])
     FactIdentifierT
      -> pure $ outputValue "int" [val]

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
 [ "if (buffer_end - buffer_ptr < " <> pretty rounded <> ") {"
 , "    error = psv_flush_output (fd, buffer, &buffer_ptr);"
 , indent 4 outputDie
 , "}"
 , vsep (fmap mkdoc swords)
 , "buffer_ptr += " <> pretty size <> ";"
 ]
 where
  swords = wordsOfString xs

  rounded  = length swords * 8
  size     = sum (fmap swSize swords)
  mkdoc sw = "*(uint64_t *)(buffer_ptr + " <> pretty (swOffset sw) <> ") = " <> swBits sw <> ";"

outputDie :: Doc
outputDie = "if (error) return error;"

quotedOutput :: IsInJSON -> Doc -> Doc
quotedOutput NotInJSON out = out
quotedOutput InJSON    out = vsep [outputChar '"', out, outputChar '"']

------------------------------------------------------------------------

schemaOfStructField :: StructField -> ValType -> Either SeaError PsvStructField
schemaOfStructField (StructField name) typ =
  PsvStructField name <$> schemaOfValType typ

schemaOfValType :: ValType -> Either SeaError PsvEncoding
schemaOfValType = \case
  BoolT ->
    pure $ PsvPrimitive PsvBoolean
  IntT ->
    pure $ PsvPrimitive PsvInt
  DoubleT ->
    pure $ PsvPrimitive PsvDouble
  StringT ->
    pure $ PsvPrimitive PsvString
  TimeT ->
    pure $ PsvPrimitive PsvString

  -- NOTE in generated C code, fact identifiers are treated as ints.
  FactIdentifierT ->
    pure $ PsvPrimitive PsvInt

  UnitT ->
    Left $ SeaUnsupportedOutputType UnitT
  ErrorT ->
    Left $ SeaUnsupportedOutputType ErrorT

  ArrayT a ->
    PsvList <$> schemaOfValType a

  -- Map is just an array of size-two-array
  MapT k v ->
    PsvList <$> (PsvPair <$> schemaOfValType k <*> schemaOfValType v)

  -- None becomes missing_value, Some is just the value
  OptionT a ->
    schemaOfValType a

  PairT a b ->
    PsvPair <$> schemaOfValType a <*> schemaOfValType b

  -- In dense PSV, an error is always missing_value
  SumT ErrorT v ->
    schemaOfValType v
  SumT x y ->
    Left $ SeaUnsupportedOutputType (SumT x y)

  StructT s ->
    PsvStruct
      <$> traverse (uncurry schemaOfStructField) (Map.toList $ getStructType s)

  BufT _ a ->
    PsvList <$> schemaOfValType a

schemaOfOutput :: OutputId -> ValType -> Either SeaError PsvColumn
schemaOfOutput outputId typ =
  PsvColumn (renderOutputId outputId) <$> schemaOfValType typ

schemaOfProgram :: Cluster c k -> Either SeaError [PsvColumn]
schemaOfProgram =
  -- NOTE the order of the columns here must match 'seaOfWriteProgramOutput' above
  traverse (\(n, MeltedType t _) -> schemaOfOutput n t) . Map.toList . clusterOutputs

schemaOfLabel :: Mode -> [PsvColumn]
schemaOfLabel = \case
  Snapshot _ ->
    []
  Chords ->
    [PsvColumn "timestamp" (PsvPrimitive PsvString)]

schemaOfFleet :: PsvOutputConfig -> [Cluster c k] -> Either SeaError PsvSchema
schemaOfFleet config clusters =
  case outputPsvFormat config of
    PsvOutputSparse ->
      Left SeaCannotGenerateSchemaForSparseOutput

    PsvOutputDense -> do
      columns0 <- concat <$> traverse schemaOfProgram clusters

      let
        columns1 =
          columns0 <> schemaOfLabel (outputPsvMode config)

        missing =
          PsvMissingValue $ outputPsvMissing config

      pure $ PsvSchema missing columns1
