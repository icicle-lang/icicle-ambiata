{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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


-- | Dictionary config can be inherited from higher level dictionaries, items such as
--   Namespace and tombstone can be scoped based on where they are defined, and overridden
--   inside a specific fact or feature.
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

-- Intermediate states so that parsing can be pure.
-- Will need to typecheck once flow through and imports are done.
data DictionaryEntry' =
  DictionaryEntry' Attribute Definition' Namespace
  deriving (Eq, Show)

type Tombstone = Text

data Definition' =
  ConcreteDefinition' Encoding (Maybe Tombstone) FactMode
  | VirtualDefinition'  Virtual'
  deriving (Eq, Show)

-- A parsed, but still to be typechecked source program.
newtype Virtual' = Virtual' {
    unVirtual' :: QueryTop Pos.SourcePos Variable
  } deriving (Eq, Show)


data DictionaryValidationError =
    UnknownElement  Text      Pos.SourcePos
  | BadType         Text Text Pos.SourcePos
  | MissingRequired Text Text
  | EncodingError   Text Text Pos.SourcePos
  | BadFactMode     Text      Pos.SourcePos
  | ParseError      ParseError
  deriving (Eq, Show)

instance Pretty DictionaryValidationError where
 pretty e
  = case e of
     UnknownElement n p
      -> vsep [ "Unknown element in dictionary:"
              , "  element : " <> pretty n
              , "  at      : " <> pretty p
              ]
     BadType n t p
      -> vsep [ "Dictionary entry has the wrong type:"
              , " entry     : " <> pretty n
              , " at        : " <> pretty p
              , " exptected : " <> pretty t
              ]
     BadFactMode n p
      -> vsep [ "Fact has unrecognised type (needs to be event or sparse/dense state):"
              , " fact : " <> pretty n
              , " at   : " <> pretty p
              ]
     MissingRequired n ex
      -> vsep [ "Dictionary entry is missing required field:"
              ,   "entry   : " <> pretty n
              ,   "missing :"  <> pretty ex
              ]
     EncodingError f n p
      -> vsep [ "Fact has a bad feature encoding:"
              , "  fact     : " <> pretty f
              , "  encoding : " <> pretty n
              , "  at       : " <> pretty p
              ]
     ParseError p
      -> vsep [ "Error parsing feature expression:"
              , indent 2 $ (text . show) p
              ]

type Name = Text

--------------------------------------------------------------------------------

tomlDict
  :: DictionaryConfig
  -> Table
  -> AccValidation [DictionaryValidationError] (DictionaryConfig, [DictionaryEntry'])
tomlDict parentConf x = fromEither $ do
  let config =   DictionaryConfig
             <$> textFocus "title" x
             <*> intFocus  "version" x
             <*> textFocus "namespace" x
             <*> textFocus "tombstone" x
             <*> textArrayFocus "import" x
             <*> textArrayFocus "chapter" x

  -- We need to treat it as a monad as facts require the config to know the default namespace.
  config' <- toEither config

  -- Join the config with its parent, so we are scoped correctly
  let config'' = config' <> parentConf

  -- Parse the facts, again, getting the Monad version at the ends.
  facts    <-  toEither
            $  maybe [] id
           <$> traverse (validateTableWith validateFact "fact" config'')
                        (x ^? key "fact")

  -- Parse features (without typechecking).
  features <-  toEither
            $  maybe [] id
           <$> traverse (validateTableWith validateFeature "feature" config'')
                        (x ^? key "feature")

  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure (config'', facts <> features)


-- | If a namespace is given, it must be validated. If not, it must have a parent value.
validateNamespace
  :: DictionaryConfig
  -> Text
  -> Table
  -> AccValidation [DictionaryValidationError] Namespace
validateNamespace conf name x =
  let valParent n
        = maybe (AccFailure [MissingRequired ("fact." <> name) n])
                AccSuccess
                (namespace conf)
      valFeatureOrParent n
        = maybe (valParent n)
                (validateText n)
                (x ^? key n)
  in Namespace <$> valFeatureOrParent "namespace"

-- | Validate a TOML node is a fact.
--
validateFact
  :: DictionaryConfig
  -> Name
  -> Table
  -> AccValidation [DictionaryValidationError] DictionaryEntry'
validateFact conf name x =
  let fname
        = "fact." <> name

      -- Every fact needs an encoding, which can't be inherited from it's parent.
      encoding
        = maybe (AccFailure [MissingRequired fname "encoding"])
                (validateEncoding' fname)
                (M.lookup "encoding" x)

      -- Fact are events unless otherwise specified
      type'
        = maybe (AccSuccess FactModeEvent) (validateFactMode fname) (M.lookup "mode" x)

      -- If a namespace is given, it must be validated. If not, it must have a parent value.
      namespace'
        = validateNamespace conf name x

      -- Tombstones are not mandatory, but can be inherited.
      tombstone'
        =   (<|> tombstone conf)
        <$> (validateText "tombstone") `traverse` (x ^? key "tombstone")

      -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  in DictionaryEntry'
       <$> pure (Attribute name)
       <*> (   ConcreteDefinition'
           <$> encoding
           <*> tombstone'
           <*> type' )
       <*> namespace'

-- | Validate a TOML node is a feature.
--   e.g.
--     [feature.mean_age]
--        expression = "feature person ~> mean age"
--
validateFeature
  :: DictionaryConfig
  -> Name
  -> Table
  -> AccValidation [DictionaryValidationError] DictionaryEntry'
validateFeature conf name x = fromEither $ do
  let fname     = "feature." <> name
      fexp      = fname <> ".expression"

  -- If a namespace is specified, validate it. Otherwise inherit the parent value.
  nsp         <- toEither $ validateNamespace conf name x


  -- A feature must specify an expression
  expression  <- maybeToRight [MissingRequired fname "expression"]
               $ x ^? key "expression"

  -- The expression must be a string
  expression' <- maybeToRight [BadType fexp "string" (expression ^. _2)]
               $ expression ^? _1 . _NTValue . _VString

  -- Run the icicle expression parser
  let toks     = lexerPositions expression'
  q           <- first (pure . ParseError)
               $ runParser (top $ OutputName name nsp) () "" toks

  -- Todo: ensure that there's no extra data lying around. All valid TOML should be used.
  pure $ DictionaryEntry' (Attribute name) (VirtualDefinition' (Virtual' q)) nsp

-- | Validate a TOML node is a fact encoding.
--
validateEncoding'
  :: Text
  -> (Node, Pos.SourcePos)
  -> AccValidation [DictionaryValidationError] Encoding

-- We can accept an encoding as a string in the old form.
-- e.g. "(location:string,severity:int)"
--
validateEncoding' ofFeature (NTValue (VString encs), pos) =
  let encodingString = pack $ fst <$> encs
  in either
       (AccFailure . const [EncodingError ofFeature encodingString pos])
        AccSuccess
       $ parseOnly parseEncoding encodingString

-- Or as a table with string fields.
--
-- e.g. [fact.encoding]
--         location="string"
--         severity="int"
--
validateEncoding' ofFeature (NTable t, _) =
  let validated name (enc, pos')
        = either AccFailure AccSuccess
        $ do -- Using a monad instance here, as the encoding should be a string.
             enc' <- maybe (Left [BadType name "string" pos'])
                           (Right . fmap fst)
                           (enc ^? _NTValue . _VString)
             -- Now that we have a string, parse it with attoparsec
             (enc'', fieldType) <-  first (const [EncodingError ofFeature (pack enc') pos'])
                                 $  parseOnly
                                      ((,) <$> parseEncoding
                                           <*> (Optional <$ char '*' <|> pure Mandatory)
                                           <*  endOfInput)
                                      (pack enc')

             pure $ StructField fieldType (Attribute name) enc''
  -- We should get an error for every failed encoding listed.
  in StructEncoding . toList <$> M.traverseWithKey validated t

-- But all other values should be failures.
validateEncoding' ofFeature (_, pos) =
  AccFailure $ [BadType (ofFeature <> ".encoding") "string" pos]

-- | Validate a TOML node is a fact type.
validateFactMode :: Text
                 -> (Node, Pos.SourcePos)
                 -> AccValidation [DictionaryValidationError] FactMode
validateFactMode fname (node, pos) = case node of
  NTValue (VString (fmap fst -> s))
   | s == "event"        -> AccSuccess FactModeEvent
   | s == "sparse_state" -> AccSuccess FactModeStateSparse
   | s == "dense_state"  -> AccSuccess FactModeStateDense
  _                      -> AccFailure [BadFactMode fname pos]


--------------------------------------------------------------------------------

-- | Validate a table, using a validator for each element.
--
validateTableWith
  :: (   DictionaryConfig
      -> Text
      -> Table
      -> AccValidation [DictionaryValidationError] a )
  -> Text
  -> DictionaryConfig
  -> (Node, Pos.SourcePos)
  -> AccValidation [DictionaryValidationError] [a]
validateTableWith validator _ conf (NTable t, _) =
  let validate name (fact', pos')
        = fromEither $ do
            -- Using a monad instance here, as the fact should be a table.
            t'' <- maybeToRight ([BadType name "table" pos']) (fact' ^? _NTable)
            -- Validate the table with the given config and function.
            toEither $ (validator conf) name t''

  -- We will get an error for every failed item listed.
  in toList <$> M.traverseWithKey validate t

validateTableWith _ n _ (_, pos) = AccFailure $ [BadType n "table" pos]


-- | Validate a TOML node is a string.
--
validateText
  :: Text
  -> (Node, Pos.SourcePos)
  -> AccValidation [DictionaryValidationError] Text
validateText ttt x
  = maybe (AccFailure [BadType ttt "string" (x ^. _2)])
          (AccSuccess . pack . fmap fst)
          (x ^? _1 . _NTValue . _VString)


-- | Validate a TOML node is an string.
--
validateInt
  :: Text
  -> (Node, Pos.SourcePos)
  -> AccValidation [DictionaryValidationError] Int64
validateInt _ (NTValue (VInteger i), _) = AccSuccess i
validateInt t (_, pos)                  = AccFailure $ [BadType t "int" pos]


-- | Validate a TOML node is an array of strings.
--
validateTextArray
  :: Text
  -> (Node, Pos.SourcePos)
  -> AccValidation [DictionaryValidationError] [Text]

validateTextArray t (NTValue (VArray xs), pos) =
  let validateString (VString x) = Right $ pack $ fst <$> x
      validateString _           = Left $ [BadType t "string" pos]
  in  either AccFailure AccSuccess $ validateString `traverse` xs

validateTextArray t (_, pos) =
  AccFailure $ [BadType t "array" pos]


textArrayFocus :: Text -> Table -> AccValidation [DictionaryValidationError] [Text]
textArrayFocus label x'
  = maybe [] id <$> traverse (validateTextArray label) (x' ^? key label)

textFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Text)
textFocus label x'
  = validateText label `traverse` (x' ^? key label)

intFocus :: Text -> Table -> AccValidation [DictionaryValidationError] (Maybe Int64)
intFocus label x'
  = validateInt label `traverse` (x' ^? key label)

--------------------------------------------------------------------------------

toEither :: AccValidation a b -> Either a b
toEither = accValidation Left Right

fromEither :: Either a b -> AccValidation a b
fromEither = either AccFailure AccSuccess

accValidation :: (a -> c) -> (b -> c) -> AccValidation a b -> c
accValidation f _ (AccFailure a) = f a
accValidation _ f (AccSuccess b) = f b


