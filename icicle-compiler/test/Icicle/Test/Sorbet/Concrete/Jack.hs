{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Sorbet.Concrete.Jack (
    X(..)
  , jRepl
  , jExtra
  , jDecl
  , jType
  , jTypeSimple
  , jExp
  , jAtom
  , jTail
  , jContext
  , jLatestSize
  , jElse
  , jAlt
  , jAltRhs
  , jAltElse
  , jGuard
  , jPat
  , jWindow
  , jWindowSize
  , jWindowUnit
  , jNeg
  , jLit
  , jPrjId
  , jVarId
  , jVarOp
  , jConId
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Generics.Geniplate (genUniverseBi')

import           Disorder.Jack (Jack)
import           Disorder.Jack (elements, frequency, sized, scale)
import           Disorder.Jack (oneOf, listOfN, maybeOf)
import           Disorder.Jack (reshrink, withShrink)

import           Icicle.Sorbet.Concrete.Syntax
import qualified Icicle.Test.Sorbet.Lexical.Jack as Lexical

import           P hiding (Alt)

-- Annotation which has a quiet show instance to reduce clutter.
data X = X
  deriving (Eq, Ord, Show)

jRepl :: Jack (Repl X)
jRepl =
  From X <$> jVarId <*> jExp

jExtra :: Jack (Extra X)
jExtra =
  Extra X <$> listOfMax1 10 jDecl

jDecl :: Jack (Decl X)
jDecl =
  oneOfRec [
      pure $ DeclEmpty X
    ] [
      DeclType X <$> jVarId <*> listOfMax 10 jConstraint <*> jType
    , DeclFun X <$> jVarId <*> listOfMax1 10 jVarId <*> jExp
    , DeclPat X <$> jPat <*> jExp
    ]

jConstraint :: Jack (Constraint X)
jConstraint =
  Constraint X
    <$> listOfMax1 10 jTypeSimple

jType :: Jack (Type X)
jType =
  Type X
    <$> listOfMax1 10 jTypeSimple
    <*> maybeOfRec jType

jTypeSimple :: Jack (TypeSimple X)
jTypeSimple =
  oneOfRec [
      TypeCon X <$> jConId
    , TypeVar X <$> jVarId
    ] [
      TypeTuple X <$> listOfMax 10 jType
    ]

jExp :: Jack (Exp X)
jExp =
  reshrink takeSubExps $
  frequencyRec [
      (1, App X <$> listOfMax1 10 jAtom <*> pure Nothing)
    ] [
      (6, In X <$> jContext <*> jExp)
    , (1, If X <$> jGuards <*> jExp <*> jElse)
    , (7, App X <$> listOfMax1 10 jAtom <*> maybeOf jTail)
    ]

jTail :: Jack (Tail X)
jTail =
  oneOf [
      Op X <$> jVarOp <*> jExp
    , Of X <$> listOfMax1 10 jAlt
    ]

jAtom :: Jack (Atom X)
jAtom =
  oneOfRec [
      Lit X <$> jLit
    , Var X <$> jVarId <*> listOfMax 10 jPrjId
    , Con X <$> jConId
    ] [
      Tuple X <$> listOfMax 10 jExp
    ]

jContext :: Jack (Context X)
jContext =
  oneOfRec [
      Latest X <$> jLatestSize
    , Windowed X <$> jWindow
    ] [
      Let X <$> listOfMax1 10 jDecl
    , Group X <$> jExp
    , Distinct X <$> jExp
    , Filter X <$> jGuards
    ]

jLatestSize :: Jack (LatestSize X)
jLatestSize =
  LatestSize X <$> Lexical.jInteger

jElse :: Jack (Else X)
jElse =
  oneOfRec [
      Else X <$> jExp
    ] [
      ElseIf X <$> jGuards <*> jExp <*> jElse
    ]
  `withShrink` \case
    ElseIf X _ x _ ->
      [Else X x]
    _ ->
      []

jAlt :: Jack (Alt X)
jAlt =
  Alt X <$> jPat <*> jAltRhs

jAltRhs :: Jack (AltRhs X)
jAltRhs =
  oneOfRec [
      AltThen X <$> jExp
    ] [
      AltIf X <$> jGuards <*> jExp <*> maybeOfRec jAltElse
    ]

jAltElse :: Jack (AltElse X)
jAltElse =
  oneOfRec [
      AltElse X <$> jExp
    ] [
      AltElseIf X <$> jGuards <*> jExp <*> maybeOfRec jAltElse
    ]

jGuards :: Jack (NonEmpty (Guard X))
jGuards =
  listOfMax1 10 jGuard

jGuard :: Jack (Guard X)
jGuard =
  oneOf [
      GuardPat X <$> jPat <*> jExp
    , GuardExp X <$> jExp
    ]

jPat :: Jack (Pat X)
jPat =
  oneOf [
      Pat1 X <$> jPat1
    , PatNeg X <$> jNeg
    , PatCon X <$> jConId <*> listOfMax1 10 jPat1
    ]

jPat1 :: Jack (Pat1 X)
jPat1 =
  oneOfRec [
      Pat1Lit X <$> jLit
    , Pat1Con X <$> jConId
    , Pat1Var X <$> jVarId
    , pure $ Pat1Wild X
    ] [
      Pat1Tuple X <$> listOfMax 10 jPat
    ]

jWindow :: Jack (Window X)
jWindow =
  oneOf [
      Fixed X <$> jWindowSize <*> jWindowUnit
    , Between X <$> jWindowSize <*> jWindowUnit <*> jWindowSize <*> jWindowUnit
    ]

jWindowSize :: Jack (WindowSize X)
jWindowSize =
  WindowSize X <$> Lexical.jInteger

jWindowUnit :: Jack (WindowUnit X)
jWindowUnit =
  elements [
      Days X
    , Months X
    , Weeks X
    ]

jNeg :: Jack (Neg X)
jNeg =
  oneOf [
      NegInteger X <$> Lexical.jInteger
    , NegRational X <$> Lexical.jRational
    ]

jLit :: Jack (Lit X)
jLit =
  oneOf [
      LitInteger X <$> Lexical.jInteger
    , LitRational X <$> Lexical.jRational
    , LitString X <$> Lexical.jString
    , LitDate X <$> Lexical.jDate
    ]

jPrjId :: Jack (PrjId X)
jPrjId =
  PrjId X <$> Lexical.jVarId

jVarId :: Jack (VarId X)
jVarId =
  VarId X <$> Lexical.jVarId

jVarOp :: Jack (VarOp X)
jVarOp =
  VarOp X <$> Lexical.jVarOp

jConId :: Jack (ConId X)
jConId =
  ConId X <$> Lexical.jConId

------------------------------------------------------------------------

listOfN' :: Int -> Int -> Jack a -> Jack [a]
listOfN' n m jack =
  sized $ \sz ->
    let
      diff_sized =
        ((m - n) * sz) `div` 100

      m_sized =
        max n (n + diff_sized)
    in
      listOfN n m_sized jack

listOfMax :: Int -> Jack a -> Jack [a]
listOfMax n =
  listOfN' 0 n

listOfMax1 :: Int -> Jack a -> Jack (NonEmpty a)
listOfMax1 n =
  fmap NonEmpty.fromList . listOfN' 1 n

maybeOfRec :: Jack a -> Jack (Maybe a)
maybeOfRec jack =
  oneOfRec [
      pure Nothing
    ] [
      Just <$> jack
    ]

oneOfRec :: [Jack a] -> [Jack a] -> Jack a
oneOfRec nonrec rec =
  sized $ \n ->
    if n <= 1 then
      oneOf nonrec
    else
      oneOf $ nonrec <> fmap (scale golden) rec

frequencyRec :: [(Int, Jack a)] -> [(Int, Jack a)] -> Jack a
frequencyRec nonrec rec =
  sized $ \n ->
    if n <= 1 then
      frequency nonrec
    else
      frequency $ nonrec <> fmap (second $ scale golden) rec

golden :: Int -> Int
golden x =
  round (fromIntegral x * 0.61803398875 :: Double)

takeSubExps :: Exp X -> [Exp X]
takeSubExps =
  drop 1 . $(genUniverseBi' [t| forall a. Exp a -> [Exp a] |])
