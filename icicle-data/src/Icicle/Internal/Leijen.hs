{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Internal.Leijen (
      module Leijen
    , (<%>)
    , renderPlain
    , renderSGR
    ) where

import           Data.String (String)
import qualified Data.Text as T

import           P

import           System.Console.ANSI (SGR(..), setSGRCode)

import           Text.PrettyPrint.Annotated.Leijen as Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Annotated.Leijen as Leijen


(<%>) :: Doc a -> Doc a -> Doc a
(<%>) =
  (Leijen.<$>)

infixr <%>

instance Monoid (Doc a) where
   mempty =
     Leijen.empty
   mappend =
     (Leijen.<>)

renderPlain :: Doc a -> Text
renderPlain =
  T.pack .
  Leijen.displayDecorated (const id) .
  Leijen.renderPretty 0.8 80

renderSGR :: forall a. (a -> [SGR]) -> Doc a -> Text
renderSGR sgrOfAnnot doc =
  let
    sdoc :: SimpleDoc a
    sdoc =
      Leijen.renderPretty 0.8 80 doc

    put :: a -> String -> String
    put ann str =
      case sgrOfAnnot ann of
        [] ->
          str
        sgr ->
          setSGRCode sgr <> str <> setSGRCode []
  in
    T.pack $ Leijen.displayDecorated put sdoc
