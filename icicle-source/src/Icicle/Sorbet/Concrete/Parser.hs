{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Concrete.Parser (
    Parser
  , pRepl
  , pExtra

  , ParserError(..)
  , renderParserError
  ) where

import           Data.List.NonEmpty (NonEmpty(..), some1)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Scientific (Scientific)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Thyme (Day)

import           Icicle.Sorbet.Concrete.Annotation
import           Icicle.Sorbet.Concrete.Syntax
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           P hiding (Alt)

import           Text.Megaparsec (Dec, try, sepBy, choice, label)
import           Text.Megaparsec (ErrorComponent(..), ShowErrorComponent(..))
import qualified Text.Megaparsec as Mega
import           Text.Megaparsec.Prim (MonadParsec)


type Parser s m =
  (MonadParsec ParserError s m, Mega.Token s ~ Positioned Token)

data ParserError =
    ParserDefault !Dec
    deriving (Eq, Ord, Show)

renderParserError :: ParserError -> Text
renderParserError = \case
  ParserDefault dec ->
    T.pack $ showErrorComponent dec

instance ErrorComponent ParserError where
  representFail =
    ParserDefault . representFail
  representIndentation old ref actual =
    ParserDefault $ representIndentation old ref actual

instance ShowErrorComponent ParserError where
  showErrorComponent =
    T.unpack . renderParserError

pRepl :: Parser s m => m (Repl Position)
pRepl =
  From
    <$> pToken Tok_From
    <*> pVarId <* pToken Tok_In
    <*> pExp <* Mega.eof

pExtra :: Parser s m => m (Extra Position)
pExtra =
  uncurry Extra
    <$> pDecls <* Mega.eof

pDecls :: Parser s m => m (Position, NonEmpty (Decl Position))
pDecls =
  (,)
    <$> pToken Tok_LBrace
    <*> (pDecl `sepBy1` pToken Tok_Semi) <* pToken Tok_RBrace

pDecl :: Parser s m => m (Decl Position)
pDecl =
  choice [
      try $ pDeclType
    , try $ pDeclFun
    , pDeclPat
    , pDeclEmpty
    ]

pDeclType :: Parser s m => m (Decl Position)
pDeclType =
  DeclType
    <%> (annotOfVarId, pVarId)
    <*> (pToken Tok_Colon *> many (try pConstraint))
    <*> pType

pDeclFun :: Parser s m => m (Decl Position)
pDeclFun =
  let
    pParam =
      label "function parameter" $
      pVarId
  in
    DeclFun
      <%> (annotOfVarId, pVarId)
      <*> some1 pParam
      <*> pDeclBody

pDeclPat :: Parser s m => m (Decl Position)
pDeclPat =
  DeclPat
    <%> (annotOfPat, pPat)
    <*> pDeclBody

pDeclBody :: Parser s m => m (Exp Position)
pDeclBody =
  label "declaration body" $
  pToken Tok_Equals *> pExp

pDeclEmpty :: Parser s m => m (Decl Position)
pDeclEmpty =
  label "empty declaration" $
  DeclEmpty
    <$> position

pConstraint :: Parser s m => m (Constraint Position)
pConstraint =
  label "constraint" $
  Constraint
    <%> (annotOfTypeSimple . NonEmpty.head, some1 pTypeSimple) <* pToken Tok_RArrowEquals

pType :: Parser s m => m (Type Position)
pType =
  label "type" $
  Type
    <%> (annotOfTypeSimple . NonEmpty.head, some1 pTypeSimple)
    <*> optional (pToken Tok_RArrowDash *> pType)

pTypeSimple :: Parser s m => m (TypeSimple Position)
pTypeSimple =
  choice [
      pTypeCon
    , pTypeVar
    , pTypeTuple
    ]

pTypeCon :: Parser s m => m (TypeSimple Position)
pTypeCon =
  label "type constructor" $
  TypeCon
    <%> (annotOfConId, pConId)

pTypeVar :: Parser s m => m (TypeSimple Position)
pTypeVar =
  label "type variable" $
  TypeVar
    <%> (annotOfVarId, pVarId)

pTypeTuple :: Parser s m => m (TypeSimple Position)
pTypeTuple =
  label "type tuple" $
  TypeTuple
    <$> pToken Tok_LParen
    <*> (pType `sepBy` pToken Tok_Comma) <* pToken Tok_RParen

pExp :: Parser s m => m (Exp Position)
pExp =
  label "expression" $
  choice [
      pExpIn
    , pExpIf
    , pExpApp
    , pExpTail
    ]

pExpIn :: Parser s m => m (Exp Position)
pExpIn =
  In
    <%> (annotOfContext, pContext) <* pToken Tok_In
    <*> pExp

pExpIf :: Parser s m => m (Exp Position)
pExpIf =
  If
    <$> pToken Tok_If
    <*> pGuards <* pToken Tok_Then
    <*> pExp
    <*> pElse

pExpApp :: Parser s m => m (Exp Position)
pExpApp = do
  App
    <%> (annotOfAtom . NonEmpty.head, some1 pAtom)
    <*> optional pTail

pExpTail :: Parser s m => m (Exp Position)
pExpTail = do
  Tail
    <%> (annotOfTail, pTail)

pTail :: Parser s m => m (Tail Position)
pTail =
  choice [
      pTailOp
    , pTailOf
    ]

pTailOp :: Parser s m => m (Tail Position)
pTailOp =
  Op
    <%> (annotOfVarOp, pVarOp)
    <*> pExp

pTailOf :: Parser s m => m (Tail Position)
pTailOf =
  Of
    <$> pToken Tok_Of <* pToken Tok_LBrace
    <*> (pAlt `sepBy1` pToken Tok_Semi) <* pToken Tok_RBrace

pAtom :: Parser s m => m (Atom Position)
pAtom =
  choice [
      pAtomLit
    , pAtomVar
    , pAtomCon
    , pAtomTuple
    ]

pAtomLit :: Parser s m => m (Atom Position)
pAtomLit =
  Lit
    <%> (annotOfLit, pLit)

pAtomVar :: Parser s m => m (Atom Position)
pAtomVar =
  Var
    <%> (annotOfVarId, pVarId)
    <*> many pPrjId

pAtomCon :: Parser s m => m (Atom Position)
pAtomCon =
  Con
    <%> (annotOfConId, pConId)

pAtomTuple :: Parser s m => m (Atom Position)
pAtomTuple =
  Tuple
    <$> pToken Tok_LParen
    <*> (pExp `sepBy` pToken Tok_Comma) <* pToken Tok_RParen

pContext :: Parser s m => m (Context Position)
pContext =
  choice [
      pContextLet
    , pContextGroup
    , pContextDistinct
    , pContextFilter
    , pContextLatest
    , pContextWindow
    ]

pContextLet :: Parser s m => m (Context Position)
pContextLet =
  Let
    <$> pToken Tok_Let
    <*> (snd <$> pDecls)

pContextGroup :: Parser s m => m (Context Position)
pContextGroup =
  Group
    <$> pToken Tok_Group
    <*> pExp

pContextDistinct :: Parser s m => m (Context Position)
pContextDistinct =
  Distinct
    <$> pToken Tok_Distinct
    <*> pExp

pContextFilter :: Parser s m => m (Context Position)
pContextFilter =
  Filter
    <$> pToken Tok_Filter
    <*> pExp

pContextLatest :: Parser s m => m (Context Position)
pContextLatest =
  Latest
    <$> pToken Tok_Latest
    <*> pLatestSize

pLatestSize :: Parser s m => m (LatestSize Position)
pLatestSize =
  uncurry LatestSize
    <$> pInteger

pContextWindow :: Parser s m => m (Context Position)
pContextWindow =
  Windowed
    <$> pToken Tok_Windowed
    <*> pWindow

pWindow :: Parser s m => m (Window Position)
pWindow =
  choice [
      pWindowBetween
    , pWindowFixed
    ]

pWindowBetween :: Parser s m => m (Window Position)
pWindowBetween =
  Between
    <$> pToken Tok_Between
    <*> pWindowSize
    <*> pWindowUnit <* pToken Tok_And
    <*> pWindowSize
    <*> pWindowUnit

pWindowFixed :: Parser s m => m (Window Position)
pWindowFixed =
  Fixed
    <%> (annotOfWindowSize, pWindowSize)
    <*> pWindowUnit

pWindowSize :: Parser s m => m (WindowSize Position)
pWindowSize =
  uncurry WindowSize
    <$> pInteger

pWindowUnit :: Parser s m => m (WindowUnit Position)
pWindowUnit =
  choice [
      pWindowDays
    , pWindowMonths
    , pWindowWeeks
    ]

pWindowDays :: Parser s m => m (WindowUnit Position)
pWindowDays =
  Days
    <$> pToken Tok_Days

pWindowMonths :: Parser s m => m (WindowUnit Position)
pWindowMonths =
  Months
    <$> pToken Tok_Months

pWindowWeeks :: Parser s m => m (WindowUnit Position)
pWindowWeeks =
  Weeks
    <$> pToken Tok_Weeks

pElse :: Parser s m => m (Else Position)
pElse =
  choice [
      pElseIf
    , pElseElse
    ]

pElseIf :: Parser s m => m (Else Position)
pElseIf =
  ElseIf
    <$> pToken Tok_If
    <*> pGuards <* pToken Tok_Then
    <*> pExp
    <*> pElse

pElseElse :: Parser s m => m (Else Position)
pElseElse =
  Else
    <$> pToken Tok_Else
    <*> pExp

pAlt :: Parser s m => m (Alt Position)
pAlt =
  Alt
    <%> (annotOfPat, pPat)
    <*> pAltRhs

pAltRhs :: Parser s m => m (AltRhs Position)
pAltRhs =
  choice [
      pAltThen
    , pAltIf
    ]

pAltThen :: Parser s m => m (AltRhs Position)
pAltThen =
  AltThen
    <$> pToken Tok_Then
    <*> pExp

pAltIf :: Parser s m => m (AltRhs Position)
pAltIf =
  AltIf
    <$> pToken Tok_If
    <*> pGuards <* pToken Tok_Then
    <*> pExp
    <*> optional pAltElse

pAltElse :: Parser s m => m (AltElse Position)
pAltElse =
  choice [
      pAltElseIf
    , pAltElseElse
    ]

pAltElseIf :: Parser s m => m (AltElse Position)
pAltElseIf =
  AltElseIf
    <$> pToken Tok_If
    <*> pGuards <* pToken Tok_Then
    <*> pExp
    <*> optional pAltElse

pAltElseElse :: Parser s m => m (AltElse Position)
pAltElseElse =
  AltElse
    <$> pToken Tok_Else
    <*> pExp

pGuards :: Parser s m => m (NonEmpty (Guard Position))
pGuards =
  pGuard `sepBy1` pToken Tok_Comma

pGuard :: Parser s m => m (Guard Position)
pGuard =
  label "guard" $
  choice [
      try pGuardPat
    , pGuardExp
    ]

pGuardPat :: Parser s m => m (Guard Position)
pGuardPat =
  label "pattern guard" $
  GuardPat
    <%> (annotOfPat, pPat) <* pToken Tok_LArrowDash
    <*> pExp

pGuardExp :: Parser s m => m (Guard Position)
pGuardExp =
  label "expression guard" $
  GuardExp
    <%> (annotOfExp, pExp)

pPat :: Parser s m => m (Pat Position)
pPat =
  label "pattern" $
  choice [
      pPatNeg
    , try pPatCon
    , pPatPat1
    ]

pPatPat1 :: Parser s m => m (Pat Position)
pPatPat1 =
  Pat1
    <%> (annotOfPat1, pPat1)

pPatNeg :: Parser s m => m (Pat Position)
pPatNeg =
  PatNeg
    <%> (annotOfNeg, pNeg)

pPatCon :: Parser s m => m (Pat Position)
pPatCon =
  PatCon
    <%> (annotOfConId, pConId)
    <*> some1 pPat1

pPat1 :: Parser s m => m (Pat1 Position)
pPat1 =
  label "pattern" $
  choice [
      pPat1Lit
    , pPat1Con
    , pPat1Var
    , pPat1Wild
    , pPat1Tuple
    ]

pPat1Lit :: Parser s m => m (Pat1 Position)
pPat1Lit =
  Pat1Lit
    <%> (annotOfLit, pLit)

pPat1Con :: Parser s m => m (Pat1 Position)
pPat1Con =
  Pat1Con
    <%> (annotOfConId, pConId)

pPat1Var :: Parser s m => m (Pat1 Position)
pPat1Var =
  Pat1Var
    <%> (annotOfVarId, pVarId)

pPat1Wild :: Parser s m => m (Pat1 Position)
pPat1Wild =
  label "wildcard '_'" $
  Pat1Wild
    <$> pToken Tok_Wild

pPat1Tuple :: Parser s m => m (Pat1 Position)
pPat1Tuple =
  label "tuple" $
  Pat1Tuple
    <$> pToken Tok_LParen
    <*> (pPat `sepBy` pToken Tok_Comma) <* pToken Tok_RParen

pNeg :: Parser s m => m (Neg Position)
pNeg =
  label "negative literal" $
  choice [
      try pNegInteger
    , pNegRational
    ]

pNegInteger :: Parser s m => m (Neg Position)
pNegInteger =
  NegInteger
    <$> pToken (Tok_VarOp "-")
    <*> (snd <$> pInteger)

pNegRational :: Parser s m => m (Neg Position)
pNegRational =
  NegRational
    <$> pToken (Tok_VarOp "-")
    <*> (snd <$> pRational)

pLit :: Parser s m => m (Lit Position)
pLit =
  label "literal" $
  choice [
      pLitInteger
    , pLitRational
    , pLitString
    , pLitDate
    ]

pLitInteger :: Parser s m => m (Lit Position)
pLitInteger =
  uncurry LitInteger
    <$> pInteger

pLitRational :: Parser s m => m (Lit Position)
pLitRational =
  uncurry LitRational
    <$> pRational

pLitString :: Parser s m => m (Lit Position)
pLitString =
  uncurry LitString
    <$> pString

pLitDate :: Parser s m => m (Lit Position)
pLitDate =
  uncurry LitDate
    <$> pDate

pPrjId :: Parser s m => m (PrjId Position)
pPrjId =
  label "projection" .
  tryToken $ \pos -> \case
    Tok_PrjId prjId ->
      Just $ PrjId pos prjId
    _ ->
      Nothing

pVarId :: Parser s m => m (VarId Position)
pVarId =
  label "variable" .
  tryToken $ \pos -> \case
    Tok_VarId varId ->
      Just $ VarId pos varId
    _ ->
      Nothing

pVarOp :: Parser s m => m (VarOp Position)
pVarOp =
  label "operator" .
  tryToken $ \pos -> \case
    Tok_VarOp varOp ->
      Just $ VarOp pos varOp
    _ ->
      Nothing

pConId :: Parser s m => m (ConId Position)
pConId =
  label "constructor" .
  tryToken $ \pos -> \case
    Tok_ConId conId ->
      Just $ ConId pos conId
    _ ->
      Nothing

pInteger :: Parser s m => m (Position, Integer)
pInteger =
  label "integer literal" .
  tryPosToken $ \case
    Tok_Integer integer ->
      Just integer
    _ ->
      Nothing

pRational :: Parser s m => m (Position, Scientific)
pRational =
  label "rational literal" .
  tryPosToken $ \case
    Tok_Rational rational ->
      Just rational
    _ ->
      Nothing

pString :: Parser s m => m (Position, Text)
pString =
  label "string literal" .
  tryPosToken $ \case
    Tok_String string ->
      Just string
    _ ->
      Nothing

pDate :: Parser s m => m (Position, Day)
pDate =
  label "date literal" .
  tryPosToken $ \case
    Tok_Date date ->
      Just date
    _ ->
      Nothing

pToken :: Parser s m => Token -> m Position
pToken tok0 =
  label ("“" <> T.unpack (renderToken tok0) <> "”") .
  --label ("‘" <> T.unpack (renderToken tok0) <> "’") .
  tryToken $ \pos tok ->
    if tok0 == tok then
      Just pos
    else
      Nothing

tryToken :: Parser s m => (Position -> Token -> Maybe a) -> m a
tryToken f  =
  tryPositioned $ \(Positioned start _ tok) ->
    f start tok

tryPosToken :: Parser s m => (Token -> Maybe a) -> m (Position, a)
tryPosToken f  =
  tryPositioned $ \(Positioned start _ tok) ->
    fmap (start,) $ f tok

tryPositioned :: Parser s m => (Positioned Token -> Maybe a) -> m a
tryPositioned f =
  let
    testToken p =
      case f p of
        Nothing ->
          Left (Set.singleton (Mega.Tokens $ p :| []), Set.empty, Set.empty)
        Just x ->
          Right x
  in
    Mega.token testToken Nothing

--
-- Like liftA2 but allows 'a' to be derived from 'b'.
--
-- Used for extracting the annotation (aka the source position) from the first
-- real constructor argument and using it as the annotation for the
-- constructor.
--
(<%>) :: Monad m => (a -> b -> c) -> (b -> a, m b) -> m c
(<%>) f (g, m) = do
  b <- m
  return $
    f (g b) b

infixl 4 <%>

sepBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep =
  (:|) <$> p <*> many (sep *> p)

position :: Parser s m => m Position
position = do
  Mega.SourcePos file line col <- Mega.getPosition
  pure $
    Position file
      (fromIntegral $ Mega.unPos line)
      (fromIntegral $ Mega.unPos col)
