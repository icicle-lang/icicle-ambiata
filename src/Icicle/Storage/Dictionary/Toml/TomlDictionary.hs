{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Storage.Dictionary.Toml.TomlDictionary
  (
    tomlDict
  , DictionaryConfig (..)
  , DictionaryEntry' (..)
  , Definition' (..)
  , Virtual' (..)
  , DictionaryValidationError (..)
  , toEither
  ) where

import           P

import           Control.Lens

import qualified Control.Applicative as CA ((<|>))
import           Data.Text
import           Data.Validation
import qualified Data.HashMap.Strict as M

import           Data.Attoparsec.Text
import qualified Text.Parsec.Pos as Pos
import           Text.Parsec (runParser)
import           Text.Parsec.Error

import           Icicle.Common.Base (OutputName(..))
import           Icicle.Data
import           Icicle.Source.Lexer.Token
import           Icicle.Source.Lexer.Lexer
import           Icicle.Source.Parser.Parser
import           Icicle.Source.Query

import           Icicle.Storage.Encoding

import           Icicle.Storage.Dictionary.Toml.Prisms
import           Icicle.Storage.Dictionary.Toml.Types

import           Icicle.Internal.Pretty hiding (char)

{-
Dictionary config can be inherited from higher level dictionaries, items such as
Namespace and tombstone can be scoped based on where they are defined, and overridden
inside a specific fact or feature.
-}


-- Intermediate states so that parsing can be pure.
-- Will need to typecheck once flow through and imports are done.
data DictionaryEntry' =
  DictionaryEntry' Attribute Definition'
  deriving (Eq, Show)

data Definition' =
    ConcreteDefinition' Namespace Encoding (Maybe Text)
  | VirtualDefinition'  Virtual'
  deriving (Eq, Show)

-- A parsed, but still to be typechecked source program.
newtype Virtual' = Virtual' {
    unVirtual' :: QueryTop Pos.SourcePos Variable
  } deriving (Eq, Show)


data DictionaryValidationError =
  UnknownElement Text Pos.SourcePos
  | BadType Text Text Pos.SourcePos
  | MissingRequired Text Text
  | EncodingError Text Text Pos.SourcePos
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
  -- Left preferenced Monoid instance.
  -- Use properties specified in this file, or, if they don't exist, try the parent.
  -- Don't bring in the imports or chapters, as that will cause an infinite loop.
  mappend
    (DictionaryConfig a1 a2 a3 a4 a6 a7)
    (DictionaryConfig b1 b2 b3 b4 _  _ ) =
      (DictionaryConfig (a1 CA.<|> b1) (a2 CA.<|> b2) (a3 CA.<|> b3) (a4 CA.<|> b4) a6 a7)

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
validateFact conf name x =
  let -- Every fact needs an encoding, which can't be inherited from it's parent.
      encoding   = maybe (AccFailure [MissingRequired ("fact." <> name) "encoding"]) (validateEncoding' ("fact." <> name)) $ M.lookup "encoding" x

      -- A parameter which is required, and if given must be validated. If it's not given however, a parent value can be used.
      valFeatureOrParent fname = (maybe (maybe (AccFailure [MissingRequired ("fact." <> name) fname]) AccSuccess (namespace conf)) (validateText fname) $ x ^? key fname)
      namespace' = Namespace <$> (valFeatureOrParent "namespace")

      -- Tombstones are not mandatory, but can be inherited.
      tombstone' = (<|> tombstone conf) <$> ((validateText "tombstone") `traverse` (x ^? key "tombstone"))

      -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  in DictionaryEntry' (Attribute name) <$> (ConcreteDefinition' <$> namespace' <*> encoding <*> tombstone')

validateEncoding' :: Text -> (Node, Pos.SourcePos) -> AccValidation [DictionaryValidationError] Encoding
-- We can accept an encoding as a string in the old form.
validateEncoding' ofFeature (NTValue (VString encs), pos) =
  let encodingString = pack $ fst <$> encs
  in either (AccFailure . const [EncodingError ofFeature encodingString pos]) AccSuccess $ parseOnly parseEncoding encodingString
-- Or as a table with string fields.
validateEncoding' ofFeature (NTable t, _) =
  -- We should get an error for every failed encoding listed.
  (StructEncoding . toList) <$> M.traverseWithKey ( \name (enc, pos') -> either AccFailure AccSuccess $ do
    -- Using a monad instance here, as the encoding should be a string, and then it should parse correctly.
    enc' <- maybe (Left $ [BadType name "string" pos']) (Right . fmap fst) $ enc ^? _NTValue . _VString
    -- Now that we have a string, parse it with attoparsec
    (enc'', fieldType) <- first (const [EncodingError ofFeature (pack enc') pos']) $ parseOnly ((,) <$> parsePrimitiveEncoding <*> (Optional <$ char '*' <|> pure Mandatory) <* endOfInput) (pack enc')
    pure $ StructField fieldType (Attribute name) enc''
  ) t
-- But all other values should be failures.
validateEncoding' ofFeature (_, pos) = AccFailure $ [BadType (ofFeature <> ".encoding") "string" pos]

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
  q      <-  first (pure . ParseError) $ runParser (top $ OutputName name) () "" toks
  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure $ DictionaryEntry' (Attribute name) (VirtualDefinition' (Virtual' q))

toEither :: AccValidation a b -> Either a b
toEither = accValidation Left Right

fromEither :: Either a b -> AccValidation a b
fromEither = either AccFailure AccSuccess

accValidation :: (a -> c) -> (b -> c) -> AccValidation a b -> c
accValidation f _ (AccFailure a) = f a
accValidation _ f (AccSuccess b) = f b


instance Pretty DictionaryValidationError where
 pretty e
  = case e of
     UnknownElement n p
      -> "Unknown element in dictionary" <+> (dquotes . pretty) n <+> "at" <+> (dquotes . pretty) p
     BadType n t p
      -> "Dictionary entry" <+> (dquotes . pretty) n <+> "at" <+> pretty p <+> "has the wrong type, expected" <+> (dquotes . pretty) t
     MissingRequired n ex
      -> "Dictionary entry" <+> (dquotes . pretty) n <+> "is missing" <+> (dquotes . pretty) ex
     EncodingError f n p
      -> "Fact" <+> (dquotes . pretty) f <+> "has a bad feature encoding" <+> (dquotes . pretty) n <+> "at" <+> pretty p <+> "it could not be parsed"
     ParseError p
      -> "Error parsing feature expression, extra info:" <+> (text . show) p
