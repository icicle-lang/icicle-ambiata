{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Storage.Dictionary.Toml.Types where

import           P

import qualified Data.HashMap.Strict as M
import           Data.List           (intersect)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    ()

import qualified Text.Parsec.Pos as Pos

-- | The 'Table' is a mapping ('HashMap') of 'Text' keys to 'Node' values.
type Table = M.HashMap Text (Node, Pos.SourcePos)

-- | A 'Node' may contain a 'TValue', or a 'Table'.
data Node = NTValue TValue
          | NTable  Table
  deriving (Eq, Show)

-- | A 'TValue' may contain any type of value that can put in a 'VArray'.
data TValue = VString   [(Char, Pos.SourcePos)]
            | VInteger  Int64
            | VFloat    Double
            | VBoolean  Bool
            | VDatetime UTCTime
            | VArray    [TValue]
  deriving (Eq, Show)

-- | Contruct an empty 'Table'.
emptyTable :: Table
emptyTable = M.empty


-- | Contruct an empty 'NTable'.
emptyNTable :: Node
emptyNTable = NTable M.empty


-- | Inserts a table ('Table') with name ('[Text]') which may be part of
-- a table array (when 'Bool' is 'True') into a 'Table'.
-- It may result in an error ('Text') on the 'Left' or a modified table
-- on the 'Right'.
insert :: ([Text], (Node, Pos.SourcePos)) -> Table -> Either Text Table
insert ([], _)           _    = Left "FATAL: Cannot call 'insert' without a name."
insert (_ , (NTValue _, _)) _ = Left "FATAL: Cannot call 'insert' with a TValue."
insert ([name], node) ttbl =
    -- In case 'name' is final
    case M.lookup name ttbl of
      Nothing           -> Right $ M.insert name node ttbl
      Just (NTable t, _)   -> case node of
        (NTable nt, pos') -> case merge t nt of
          Left ds -> Left $ T.concat [ "Cannot redefine key(s) (", (T.intercalate ", " ds)
                                     , "), from table named '", name, "'." ]
          Right r -> Right $ M.insert name (NTable r, pos') ttbl
        _         -> commonInsertError node [name]
      Just _            -> commonInsertError node [name]
insert (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final, but a sub-name
    case M.lookup name ttbl of
      Nothing           -> case insert (ns, node) emptyTable of
                             Left msg -> Left msg
                             Right r  -> Right $ M.insert name (NTable r, snd node) ttbl
      Just (NTable t, pos') -> case insert (ns, node) t of
                             Left msg -> Left msg
                             Right tt -> Right $ M.insert name (NTable tt, pos') ttbl
      Just _            -> commonInsertError node fullName


-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contian those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: Table -> Table -> Either [Text] Table
merge existing new = case intersect (M.keys existing) (M.keys new) of
                       [] -> Right $ M.union existing new
                       ds -> Left  $ ds


-- | Convenience function to construct a common error message for the 'insert' function.
commonInsertError :: (Node, Pos.SourcePos) -> [Text] -> Either Text Table
commonInsertError (what, _) name = Left . T.concat $ case what of
    NTValue _ -> ["Cannot insert a value '", n, "'."]
    _           -> ["Cannot insert ", w, " '", n, "' as key already exists."]
  where
    n = T.intercalate "." name
    w = case what of (NTable _) -> "tables"
                     _          -> "array of tables"

