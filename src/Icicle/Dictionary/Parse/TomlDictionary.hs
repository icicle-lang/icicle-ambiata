{-# LANGUAGE OverloadedStrings #-}

module Icicle.Dictionary.Parse.TomlDictionary
  (
    tomlDict
  , DictionaryConfig (..)
  , DictionaryValidationError (..)
  , DictionaryEntry' (..)
  ) where

import           P

import           Control.Lens

import qualified Control.Applicative as CA ((<|>))
import           Data.Either.Combinators
import           Data.Text
import           Data.Validation
import qualified Data.HashMap.Strict as M

import           Data.Attoparsec.Text
import qualified Text.Parsec.Pos as Pos

import           Icicle.Data
import           Icicle.Source.Lexer.Token
import           Icicle.Source.Lexer.Lexer

import           Icicle.Dictionary.Data
import           Icicle.Dictionary.Parse
import           Icicle.Dictionary.Parse.Prisms
import           Icicle.Dictionary.Parse.Toml

{-
Dictionary config can be inherited from higher level dictionaries, items such as
Namespace and tombstone can be scoped based on where they are defined, and overridden
inside a specific fact or feature.
-}

data DictionaryValidationError =
  UnknownElement Text Pos.SourcePos
  | BadType Text Text Pos.SourcePos
  | MissingRequired Text Text
  | ParseError Pos.SourcePos Text
  deriving (Eq, Show)

data DictionaryEntry' =
  DictionaryEntryConcrete' Attribute Definition Text Text {- Text -}
  | DictionaryEntryVirtual' Attribute [TOK]
   deriving (Eq, Show)

data DictionaryConfig =
  DictionaryConfig {
    title     :: Maybe Text
  , version   :: Maybe Int64
  , namespace :: Maybe Text
  , tombstone :: Maybe Text
  -- , mode      :: Maybe Text
  , imports   :: [Text]
  , chapter   :: [Text]
  } deriving (Eq, Show)

instance Monoid DictionaryConfig where
  mempty  = DictionaryConfig Nothing Nothing Nothing Nothing {-(Just "state")-} [] []
  mappend
    (DictionaryConfig a1 a2 a3 a4 {-a5-} a6 a7)
    (DictionaryConfig b1 b2 b3 b4 {-b5-} b6 b7) =
      (DictionaryConfig (b1 CA.<|> a1) (b2 CA.<|> a2) (b3 CA.<|> a3) (b4 CA.<|> a4) {-(a5 CA.<|> b5)-} (a6 <> b6) (a7 <> b7))

tomlDict :: DictionaryConfig -> Table -> AccValidation [DictionaryValidationError] (DictionaryConfig, [DictionaryEntry'])
tomlDict parentConf x = either AccFailure AccSuccess $ do
  -- Potentially acquire the dictionary configuration items
  let title'     = textFocus "title" x
  let version'   = intFocus  "version" x
  let namespace' = textFocus "namespace" x
  let tombstone' = textFocus "tombstone" x
  -- let mode'      = textFocus "mode" x
  -- If either of these are missing that's fine, there's just no imports/chapters
  let imports'   = (maybe [] id) <$> (traverse (validateTextArray "import") $ x ^? key "import")
  let chapters'  = (maybe [] id) <$> (traverse (validateTextArray "chapter") $ x ^? key"chapter")
  -- Config will contain all errors built up.
  let config     = DictionaryConfig <$> title' <*> version' <*> namespace' <*> tombstone' {- <*> mode' -} <*> imports' <*> chapters'
  -- We need to treat it as a monad as facts requires the config to know the default namespace.
  config' <- accValidation Left Right config
  -- Join the config with its parent, so we are scoped correctly
  let config'' = config' <> parentConf
  -- Parse the facts, again, getting the Monad version at the ends.
  facts    <- accValidation Left Right $ (maybe [] id) <$> (traverse (validateTableWith validateFact "fact" config'') $ x ^? key "fact")
  features <- accValidation Left Right $ (maybe [] id) <$> (traverse (validateTableWith validateFeature "feature" config'') $ x ^? key "feature")
  pure (config'', facts <> features)
    where
      textFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Text)
      textFocus label x' = (validateText label) `traverse` (x' ^? key label)
      intFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Int64)
      intFocus label x' = (validateInt label) `traverse` (x' ^? key label)

validateTableWith :: (DictionaryConfig -> Text -> Table -> AccValidation [DictionaryValidationError] a)
                  -> Text -> DictionaryConfig -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] [a]
validateTableWith f _ conf (NTable t, _) =
  -- We will get an error for every failed item listed.
  toList <$> M.traverseWithKey ( \name (fact', pos') -> either AccFailure AccSuccess $ do
    -- Using a monad instance here, as the fact should be a table, and it should then also parse correctly.
    t'' <- maybe (Left $ [BadType name "table" pos']) Right $ fact' ^? _NTable
    -- Validate the table with the given config and function.
    accValidation Left Right $ (f conf) name t''
  ) t
validateTableWith _ n _ (_, pos) = AccFailure $ [BadType n "table" pos]

validateFact :: DictionaryConfig -> Text -> Table -> AccValidation [DictionaryValidationError] DictionaryEntry'
validateFact conf name x =
  let -- A parameter which is required, and if given must be validated. If it's not given however, a parent value can be used.
      valFeatureOrParent fname = (maybe (maybe (AccFailure [MissingRequired ("fact." <> name) fname]) AccSuccess (namespace conf)) (validateText fname) $ x ^? key fname)
      -- Every fact needs an encoding, which can't be inherited from it's parent.
      encoding   = maybe (AccFailure [MissingRequired ("fact." <> name) "encoding"]) validateEncoding' $ M.lookup "encoding" x
      namespace' = valFeatureOrParent "namespace"
      tombstone' = valFeatureOrParent "tombstone"
      -- mode'      = valFeatureOrParent "mode"
  in DictionaryEntryConcrete' (Attribute name) <$> (ConcreteDefinition <$> encoding) <*> namespace' <*> tombstone' {-<*> mode'-}

validateEncoding' :: (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] Encoding
-- We can accept an encoding as a string in the old form.
validateEncoding' (NTValue (VString encs), pos) =
  let encodingString = pack $ fst <$> encs
  in either (AccFailure . pure . ParseError pos . pack) AccSuccess $ parseOnly parseEncoding encodingString
-- Or as a table with string fields.
validateEncoding' (NTable t, _) =
  -- We should get an error for every failed encoding listed.
  (StructEncoding . toList) <$> M.traverseWithKey ( \name (enc, pos') -> either AccFailure AccSuccess $ do
    -- Using a monad instance here, as the encoding should be a string, and then it should parse correctly.
    enc' <- maybe (Left $ [BadType name "string" pos']) (Right . fmap fst) $ enc ^? _NTValue . _VString
    -- Now that we have a string, parse it with attoparsec
    (enc'', fieldType) <- mapLeft (pure . ParseError pos' . pack) $ parseOnly ((,) <$> parsePrimitiveEncoding <*> (Optional <$ char '*' <|> pure Mandatory) <* endOfInput) (pack enc')
    pure $ StructField fieldType (Attribute name) enc''
  ) t
-- But all other values should be failures.
validateEncoding' (_, pos) = AccFailure $ [BadType "encoding" "string" pos]

validateText :: Text -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] Text
validateText ttt x = maybe (AccFailure [BadType ttt "string" (x ^. _2)]) (AccSuccess . pack . fmap fst) $ x ^? _1 . _NTValue . _VString

validateInt :: Text -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] Int64
validateInt _ (NTValue (VInteger i), _) = AccSuccess i
validateInt ttt (_, pos) = AccFailure $ [BadType ttt "int" pos]

validateTextArray :: Text -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] [Text]
validateTextArray ttt (NTValue (VArray xs), pos) =
  let f (VString x) = Right $ pack $ fst <$> x
      f _ = Left $ [BadType ttt "string" pos]
  in  either AccFailure AccSuccess $ f `traverse` xs
-- We expected a different type.
validateTextArray ttt (_, pos) = AccFailure $ [BadType ttt "array" pos]

validateFeature :: DictionaryConfig -> Text -> Table -> AccValidation [DictionaryValidationError] DictionaryEntry'
validateFeature _ name x = either AccFailure AccSuccess $ do
  expression  <- maybe (Left [MissingRequired ("feature." <> name) "expression"]) Right $ x ^? key "expression"
  expression' <- maybe (Left [BadType ("feature." <> name <> ".expression") "string" (expression ^. _2)]) Right $ expression ^? _1 . _NTValue . _VString
  let lexed = lexerPositions expression'
  pure $ DictionaryEntryVirtual' (Attribute name) lexed

accValidation :: (a -> c) -> (b -> c) -> AccValidation a b -> c
accValidation f _ (AccFailure a) = f a
accValidation _ f (AccSuccess b) = f b
