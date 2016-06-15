{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO.Base.Input
  ( -- * Input options
    InputOpts (..)
  , InputAllowDupTime (..)

    -- * C gen
  , CStmt
  , CBlock
  , CName
  , CFun
  , Tombstones
  , SeaInput (..)
  , SeaInputError (..)
  , seaOfReadNamedFact
  , initType
  , Assignment
  , assignVar
  , assignArrayMutable
  , seaOfArrayPutMutable
  , seaOfStringEq
  , seaOfBytesEq
  , CheckedInput (..)
  , checkInputType
  , FieldMapping (..)
  , mappingOfFields

    -- * Helpers
  , last
  , init
  , sizeOfString
  , wrapInBlock
  , unArray
  ) where

import qualified Data.ByteString                  as B
import           Data.Map                         (Map)
import           Data.Set                         (Set)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Word                        (Word8)

import           Icicle.Avalanche.Prim.Flat       (Prim(..), PrimArray(..))
import           Icicle.Avalanche.Prim.Flat       (meltType)

import           Icicle.Common.Type               (ValType(..), StructField(..))
import           Icicle.Common.Type               (defaultOfType)

import           Icicle.Data                      (Attribute(..))

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error                 (SeaError(..))
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Program (seaOfXValue)
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Psv.Base

import           P


type Name = Text

data InputOpts = InputOpts
  { inputAllowDupTime :: InputAllowDupTime
  , inputTombstones   :: Map Attribute (Set Text)
  } deriving (Show, Eq)

-- | Whether fact times must be unique for an entity.
--
data InputAllowDupTime
  = AllowDupTime | DoNotAllowDupTime
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

type Tombstone  = Text
type Tombstones = Set Tombstone

data CheckedInput = CheckedInput {
    inputSumError :: Name
  , inputTime     :: Name
  , inputType     :: ValType
  , inputVars     :: [(Name, ValType)]
  } deriving (Eq, Ord, Show)

checkInputType :: SeaProgramState -> Either SeaError CheckedInput
checkInputType state
 = case stateInputType state of
     PairT (SumT ErrorT t) TimeT
      | (sumError, ErrorT) : xs0 <- stateInputVars state
      , Just vars                <- init xs0
      , Just (time, TimeT)       <- last xs0
      -> Right CheckedInput {
             inputSumError = newPrefix <> sumError
           , inputTime     = newPrefix <> time
           , inputType     = t
           , inputVars     = fmap (first (newPrefix <>)) vars
           }

     t
      -> Left (SeaUnsupportedInputType t)

--------------------------------------------------------------------------------

type CStmt  = Doc -- lies
type CBlock = Doc
type CFun   = Doc
type CName  = Doc

-- Common input errors, for both PSV and Zebra
data SeaInputError = SeaInputError
  { seaInputErrorTimeOutOfOrder   :: CStmt
    -- ^ what to do when time is out of order
  , seaInputErrorCountExceedLimit :: CStmt
    -- ^ what to do when ent-attr count exceeds limit
  }

-- Common input statements, for both PSV and Zebra
data SeaInput = SeaInput
  { cstmtReadFact     :: SeaProgramState -> Tombstones -> CheckedInput -> CStmt -> CStmt -> CFun
  -- ^ Generate C code to read input into the `program->input` struct.
  , cstmtReadTime     :: CStmt
  -- ^ Generate C code to read the current fact time.
  , cfunReadTombstone :: CheckedInput -> [Tombstone] -> CStmt
  -- ^ Generate C code to read the tombstone of this input.
  , cnameFunReadFact  :: SeaProgramState -> CName
  -- ^ Name of the read_fact function. e.g. psv_read_fact_0.
  }

seaOfReadNamedFact :: SeaInput
                   -> SeaInputError
                   -> InputAllowDupTime
                   -> SeaProgramState
                   -> CStmt
seaOfReadNamedFact funs errs allowDupTime state
 = let fun    = cnameFunReadFact funs  state
       pname  = pretty (nameOfProgram  state)
       tname  = pretty (nameOfLastTime state)
       cname  = pretty (nameOfCount    state)
       tcond  = if allowDupTime == AllowDupTime
                then "if (time < last_time)"
                else "if (time <= last_time)"
   in vsep
      [ "{"
      , "    itime_t time;"
      , indent 4 $ cstmtReadTime funs
      , ""
      , "    ibool_t        ignore_time = itrue;"
      , "    iint_t         chord_count = fleet->chord_count;"
      , "    const itime_t *chord_times = fleet->chord_times;"
      , ""
      , "    /* ignore this time if it comes after all the chord times */"
      , "    for (iint_t chord_ix = 0; chord_ix < chord_count; chord_ix++) {"
      , "        if (chord_times[chord_ix] >= time) {"
      , "            ignore_time = ifalse;"
      , "            break;"
      , "        }"
      , "    }"
      , ""
      , "    if (ignore_time) return 0;"
      , ""
      , "    itime_t last_time = fleet->" <> tname <> ";"
      , ""
      , indent 4 tcond
      , "    {"
      , "        char curr_time_ptr[text_itime_max_size];"
      , "        size_t curr_time_size = text_write_itime (time, curr_time_ptr);"
      , ""
      , "        char last_time_ptr[text_itime_max_size];"
      , "        size_t last_time_size = text_write_itime (last_time, last_time_ptr);"
      , ""
      , indent 8 $ seaInputErrorTimeOutOfOrder errs
      , "    }"
      , ""
      , "    fleet->" <> tname <> " = time;"
      , ""
      , "    fleet->" <> cname <> " += 1;"
      , ""
      , "    if (fleet->" <> cname <> " > max_ent_attr_count)"
      , "    {"
      , indent 8 $ seaInputErrorCountExceedLimit errs
      , "    }"
      , ""
      , "    return " <> fun <> " (value_ptr, value_size, time, fleet->mempool, chord_count, fleet->" <> pname <> ");"
      , ""
      , "}"
      , ""
      ]

--------------------------------------------------------------------------------

initType :: ValType -> Doc
initType vt = " = " <> seaOfXValue (defaultOfType vt) vt <> ";"

-- Describes how to assign to a C struct member, this changes for arrays
type Assignment = Doc -> ValType -> Doc -> Doc

assignVar :: Assignment
assignVar n _ x = pretty n <+> "=" <+> x <> ";"

assignArrayMutable :: Assignment
assignArrayMutable n t x = n <+> "=" <+> seaOfArrayPutMutable n "ix" x t <> ";"

seaOfArrayPutMutable :: Doc -> Doc -> Doc -> ValType -> Doc
seaOfArrayPutMutable arr ix val typ
 = seaOfPrimDocApps (seaOfXPrim (PrimArray (PrimArrayPutMutable typ)))
                    [ arr, ix, val ]

seaOfStringEq :: Text -> Doc -> Maybe Doc -> Doc
seaOfStringEq str ptr msize
 | Just size <- msize = align (vsep [szdoc size, cmpdoc])
 | otherwise          = align cmpdoc
 where
   nbytes = length bytes
   bytes  = B.unpack (T.encodeUtf8 str)

   szdoc size = size <+> "==" <+> int nbytes <+> "&&"
   cmpdoc     = seaOfBytesEq bytes ptr

seaOfBytesEq :: [Word8] -> Doc -> Doc
seaOfBytesEq bs ptr
 = vsep . punctuate " &&" . fmap go $ wordsOfBytes bs
 where
   go (StringWord off _ mask bits)
    = "(*(uint64_t *)(" <> ptr <+> "+" <+> int off <> ") &" <+> mask <> ") ==" <+> bits

--------------------------------------------------------------------------------

data FieldMapping = FieldMapping {
    _fieldName :: Text
  , _fieldType :: ValType
  , _fieldVars :: [(Text, ValType)]
  } deriving (Eq, Ord, Show)

mappingOfFields :: [(StructField, ValType)] -> [(Text, ValType)] -> Maybe [FieldMapping]
mappingOfFields []     []  = pure []
mappingOfFields []     _   = Nothing
mappingOfFields (f:fs) vs0 = do
  (m,  vs1) <- mappingOfField  f  vs0
  ms        <- mappingOfFields fs vs1
  pure (m : ms)

mappingOfField :: (StructField, ValType) -> [(Text, ValType)] -> Maybe (FieldMapping, [(Text, ValType)])
mappingOfField (StructField fname, ftype) vars0 = do
  let go t (n, t')
       | t == t'   = Just (n, t)
       | otherwise = Nothing

  ns <- zipWithM go (meltType ftype) vars0

  let mapping = FieldMapping fname ftype ns
      vars1   = drop (length ns) vars0

  return (mapping, vars1)

--------------------------------------------------------------------------------

wrapInBlock :: Doc -> Doc
wrapInBlock x
  = vsep ["{", indent 4 x, "}"]

sizeOfString :: Text -> Int
sizeOfString = B.length . T.encodeUtf8

unArray :: (Text, ValType) -> Either SeaError (Text, ValType)
unArray (n, ArrayT t) = Right (n, t)
unArray (n, t)        = Left (SeaInputTypeMismatch t [(n, t)])

last :: [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x:) <$> init xs
