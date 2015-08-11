{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Dictionary.Parse.TomlDictionary
  (
    tomlDict
  , DictionaryConfig (..)
  , DictionaryValidationError (..)
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
import           Text.Parsec (runParser)
import           Text.Parsec.Error


import           Icicle.Data
import           Icicle.Source.Lexer.Lexer
import           Icicle.Source.Parser.Parser

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
  | EncodingError Text Pos.SourcePos
  | ParseError ParseError
  deriving (Eq, Show)

data DictionaryConfig =
  DictionaryConfig {
    title     :: Maybe Text
  , version   :: Maybe Int64
  , namespace :: Maybe Text
  , tombstone :: Maybe Text
  , imports   :: [Text]
  , chapter   :: [Text]
  } deriving (Eq, Show)

instance Monoid DictionaryConfig where
  mempty  = DictionaryConfig Nothing Nothing Nothing Nothing [] []
  mappend
    (DictionaryConfig a1 a2 a3 a4 a6 a7)
    (DictionaryConfig b1 b2 b3 b4 b6 b7) =
      (DictionaryConfig (b1 CA.<|> a1) (b2 CA.<|> a2) (b3 CA.<|> a3) (b4 CA.<|> a4) (a6 <> b6) (a7 <> b7))

tomlDict :: DictionaryConfig -> Table -> AccValidation [DictionaryValidationError] (DictionaryConfig, [DictionaryEntry'])
tomlDict parentConf x = fromEither $ do
  -- Potentially acquire the dictionary configuration items
  let title'     = textFocus "title" x
  let version'   = intFocus  "version" x
  let namespace' = textFocus "namespace" x
  let tombstone' = textFocus "tombstone" x
  -- If either of these are missing that's fine, there's just no imports/chapters
  let imports'   = textArrayFocus "import" x
  let chapters'  = textArrayFocus "chapter" x
  -- Config will contain all errors built up.
  let config     = DictionaryConfig <$> title' <*> version' <*> namespace' <*> tombstone' <*> imports' <*> chapters'
  -- We need to treat it as a monad as facts require the config to know the default namespace.
  config' <- toEither config
  -- Join the config with its parent, so we are scoped correctly
  let config'' = config' <> parentConf
  -- Parse the facts, again, getting the Monad version at the ends.
  facts    <- toEither $ (maybe [] id) <$> (traverse (validateTableWith validateFact "fact" config'') $ x ^? key "fact")
  -- Superficially parse the features, we haven't imported our functions yet, so can't type check them at the moment.
  features <- toEither $ (maybe [] id) <$> (traverse (validateTableWith validateFeature "feature" config'') $ x ^? key "feature")
  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure (config'', facts <> features)
    where
      textArrayFocus :: Text -> Table -> AccValidation [DictionaryValidationError] [Text]
      textArrayFocus label x' = (maybe [] id) <$> (traverse (validateTextArray label) $ x' ^? key label)
      textFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Text)
      textFocus label x' = (validateText label) `traverse` (x' ^? key label)
      intFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Int64)
      intFocus label x' = (validateInt label) `traverse` (x' ^? key label)

validateTableWith :: (DictionaryConfig -> Text -> Table -> AccValidation [DictionaryValidationError] a)
                  -> Text -> DictionaryConfig -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] [a]
validateTableWith f _ conf (NTable t, _) =
  -- We will get an error for every failed item listed.
  toList <$> M.traverseWithKey ( \name (fact', pos') -> fromEither $ do
    -- Using a monad instance here, as the fact should be a table, and it should then also parse correctly.
    t'' <- maybe (Left $ [BadType name "table" pos']) Right $ fact' ^? _NTable
    -- Validate the table with the given config and function.
    toEither $ (f conf) name t''
  ) t
validateTableWith _ n _ (_, pos) = AccFailure $ [BadType n "table" pos]

validateFact :: DictionaryConfig -> Text -> Table -> AccValidation [DictionaryValidationError] DictionaryEntry'
validateFact _ name x =
  let -- Every fact needs an encoding, which can't be inherited from it's parent.
      encoding   = maybe (AccFailure [MissingRequired ("fact." <> name) "encoding"]) validateEncoding' $ M.lookup "encoding" x
      {-
         This section is commented out until concrete features can specify their tombstones and namespaces are first class.
         -- A parameter which is required, and if given must be validated. If it's not given however, a parent value can be used.
         valFeatureOrParent fname = (maybe (maybe (AccFailure [MissingRequired ("fact." <> name) fname]) AccSuccess (namespace conf)) (validateText fname) $ x ^? key fname)
         namespace' = valFeatureOrParent "namespace"
         tombstone' = valFeatureOrParent "tombstone"
      -}
      -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  in DictionaryEntry' (Attribute name) <$> (ConcreteDefinition' <$> encoding)

validateEncoding' :: (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] Encoding
-- We can accept an encoding as a string in the old form.
validateEncoding' (NTValue (VString encs), pos) =
  let encodingString = pack $ fst <$> encs
  in either (AccFailure . const [EncodingError encodingString pos]) AccSuccess $ parseOnly parseEncoding encodingString
-- Or as a table with string fields.
validateEncoding' (NTable t, _) =
  -- We should get an error for every failed encoding listed.
  (StructEncoding . toList) <$> M.traverseWithKey ( \name (enc, pos') -> either AccFailure AccSuccess $ do
    -- Using a monad instance here, as the encoding should be a string, and then it should parse correctly.
    enc' <- maybe (Left $ [BadType name "string" pos']) (Right . fmap fst) $ enc ^? _NTValue . _VString
    -- Now that we have a string, parse it with attoparsec
    (enc'', fieldType) <- mapLeft (const [EncodingError (pack enc') pos']) $ parseOnly ((,) <$> parsePrimitiveEncoding <*> (Optional <$ char '*' <|> pure Mandatory) <* endOfInput) (pack enc')
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
validateFeature _ name x = fromEither $ do
  expression  <- maybe (Left [MissingRequired ("feature." <> name) "expression"]) Right $ x ^? key "expression"
  expression' <- maybe (Left [BadType ("feature." <> name <> ".expression") "string" (expression ^. _2)]) Right $ expression ^? _1 . _NTValue . _VString
  let toks = lexerPositions expression'
  q      <-  mapLeft (pure . ParseError) $ runParser top () "" toks
  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure $ DictionaryEntry' (Attribute name) (VirtualDefinition' (Virtual' q))

toEither :: AccValidation a b -> Either a b
toEither = accValidation Left Right

fromEither :: Either a b -> AccValidation a b
fromEither = either AccFailure AccSuccess

accValidation :: (a -> c) -> (b -> c) -> AccValidation a b -> c
accValidation f _ (AccFailure a) = f a
accValidation _ f (AccSuccess b) = f b
