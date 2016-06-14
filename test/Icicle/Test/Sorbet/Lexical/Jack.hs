{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Sorbet.Lexical.Jack (
    jToken
  , jInteger
  , jRational
  , jString
  , jDate
  , jYearMonthDay
  , jVarId
  , jConId
  , jVarIdHead
  , jConIdHead
  , jIdTail
  , jVarOp
  , jConOp
  , jVarOpHead
  , jConOpHead
  , jOpTail
  , jHeadTail
  , notReserved
  , reserved
  ) where

import           Data.Scientific (Scientific, scientific)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Thyme (Day, YearMonthDay(..), gregorianValid)

import           Disorder.Core.Gen (genValidUtf8, shrinkValidUtf8)
import           Disorder.Jack (Jack, mkJack)
import           Disorder.Jack (choose, chooseChar, oneOf, frequency, elements, listOf)
import           Disorder.Jack (suchThat, justOf, arbitrary, sized)

import           Icicle.Sorbet.Lexical.Syntax

import           P

import           Test.QuickCheck.Instances ()


jToken :: Jack Token
jToken =
  oneOf [
      pure Tok_LParen
    , pure Tok_RParen
    , pure Tok_LBrace
    , pure Tok_RBrace
    , pure Tok_LBracket
    , pure Tok_RBracket
    , pure Tok_Semi
    , pure Tok_Comma
    , pure Tok_Backtick
    , pure Tok_LArrowDash
    , pure Tok_RArrowDash
    , pure Tok_RArrowEquals
    , pure Tok_Equals
    , pure Tok_Colon
    , pure Tok_At
    , pure Tok_Of
    , pure Tok_If
    , pure Tok_Then
    , pure Tok_Else
    , pure Tok_From
    , pure Tok_In
    , pure Tok_Let
    , pure Tok_Windowed
    , pure Tok_Group
    , pure Tok_Distinct
    , pure Tok_Filter
    , pure Tok_Latest
    , pure Tok_Between
    , pure Tok_And
    , pure Tok_Days
    , pure Tok_Months
    , pure Tok_Weeks
    , Tok_PrjId <$> jVarId
    , Tok_VarId <$> jVarId
    , Tok_ConId <$> jConId
    , Tok_VarOp <$> jVarOp
    , Tok_ConOp <$> jConOp
    , Tok_Integer <$> jInteger
    , Tok_Rational <$> jRational
    , Tok_String <$> jString
    , Tok_Date <$> jDate
    ]

jInteger :: Jack Integer
jInteger =
  abs <$> arbitrary

jRational :: Jack Scientific
jRational =
  scientific
    <$> maxSized 99999999999999999
    <*> arbitrary

maxSized :: Double -> Jack Integer
maxSized x_max =
  sized $ \n ->
    let
      pct = fromIntegral n / 100.0
    in
      choose (0, round . exp $ log x_max * pct)

jString :: Jack Text
jString =
  mkJack shrinkValidUtf8 genValidUtf8

jDate :: Jack Day
jDate =
  justOf $ fmap gregorianValid jYearMonthDay

jYearMonthDay :: Jack YearMonthDay
jYearMonthDay =
  YearMonthDay
    <$> choose (0, 9999)
    <*> choose (1, 12)
    <*> choose (1, 31)

jVarId :: Jack Text
jVarId =
  jHeadTail jVarIdHead jIdTail

jConId :: Jack Text
jConId =
  jHeadTail jConIdHead jIdTail

jVarIdHead :: Jack Char
jVarIdHead =
  frequency [
      (26, chooseChar ('a', 'z'))
    , (1, pure '_')
    ]

jConIdHead :: Jack Char
jConIdHead =
  frequency [
      (26, chooseChar ('A', 'Z'))
    ]

jIdTail :: Jack Char
jIdTail =
  frequency [
      (26, chooseChar ('a', 'z'))
    , (26, chooseChar ('A', 'Z'))
    , (10, chooseChar ('0', '9'))
    , (1, pure '_')
    , (1, pure '\'')
    ]

jVarOp :: Jack Text
jVarOp =
  jHeadTail jVarOpHead jOpTail

jConOp :: Jack Text
jConOp =
  jHeadTail jConOpHead jOpTail

jVarOpHead :: Jack Char
jVarOpHead =
  elements "!#$%&*+./<=>?@\\^-~|"

jConOpHead :: Jack Char
jConOpHead =
  pure ':'

jOpTail :: Jack Char
jOpTail =
  elements ":!#$%&*+./<=>?@\\^-~|"

jHeadTail :: Jack Char -> Jack Char -> Jack Text
jHeadTail jHead jTail =
  notReserved . fmap T.pack $
    (:) <$> jHead <*> listOf jTail

notReserved :: Jack Text -> Jack Text
notReserved j =
  suchThat j $ \txt ->
    not (Set.member txt reserved) &&
    not ("--" `T.isPrefixOf` txt) -- operators can't start with comment

reserved :: Set Text
reserved =
  Set.fromList [
      "<-"
    , "->"
    , "=>"
    , "="
    , ":"
    , "@"
    , "_"
    , "of"
    , "if"
    , "then"
    , "else"
    , "from"
    , "in"
    , "let"
    , "windowed"
    , "group"
    , "distinct"
    , "filter"
    , "latest"
    , "between"
    , "and"
    , "days"
    , "months"
    , "weeks"
    ]
