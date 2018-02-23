{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sorbet.Concrete.Pretty (
    Layout(..)

  , DocAnn(..)
  , sgrOfDocAnn

  , ppRepl
  , ppExtra
  , ppDecl
  , ppType
  , ppTypeSimple
  , ppExp
  , ppTail
  , ppAtom
  , ppContext
  , ppLatestSize
  , ppElse
  , ppAlt
  , ppAltRhs
  , ppAltIf
  , ppAltElse
  , ppGuards
  , ppGuard
  , ppPat
  , ppPat1
  , ppWindow
  , ppWindowSize
  , ppWindowUnit
  , ppNeg
  , ppLit
  , ppVarId
  , ppVarOp
  , ppConId
  ) where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Scientific (Scientific)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as T
import           Data.Thyme (Day, showGregorian)

import           Icicle.Internal.Leijen (Doc, (<+>), (<%>))
import           Icicle.Internal.Leijen (align, vsep, hcat, hsep, indent, renderPlain)
import qualified Icicle.Internal.Leijen as Leijen

import           Icicle.Sorbet.Concrete.Syntax
import           Icicle.Sorbet.Lexical.Escape

import           P hiding (Alt, exp)

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import           System.Console.ANSI (ConsoleLayer(..))
import           System.Console.ANSI (SGR(..))


data Layout =
    Spaces
  | Braces
    deriving (Eq, Ord, Show)

data DocAnn a =
    DocAnn !a
  | DocKeyword
  | DocTemporal
  | DocDelimiter
  | DocOperator
  | DocLiteral
  | DocUseCon
  | DocUseVar
  | DocParVar
  | DocDefVar
    deriving (Eq, Ord, Show)

sgrOfDocAnn :: DocAnn a -> [SGR]
sgrOfDocAnn = \case
  DocAnn _ ->
    []
  DocKeyword ->
    [SetColor Foreground Dull Blue]
  DocTemporal ->
    [SetColor Foreground Vivid Blue]
  DocDelimiter ->
    [SetColor Foreground Vivid Black]
  DocLiteral ->
    [SetColor Foreground Dull Red]
  DocOperator ->
    [SetColor Foreground Dull Yellow]
  DocUseCon ->
    [SetColor Foreground Dull Green]
  DocUseVar ->
    [SetColor Foreground Dull White]
  DocParVar ->
    [SetColor Foreground Dull Magenta]
  DocDefVar ->
    [SetColor Foreground Dull Cyan]

ppRepl :: Layout -> Repl a -> Doc (DocAnn a)
ppRepl layout = \case
  From a var exp ->
    annotate a $
      ppKeyword "from" <+> useVarId var <+> ppKeyword "in" <%>
      ppExp layout exp

ppExtra :: Layout -> Extra a -> Doc (DocAnn a)
ppExtra layout = \case
  Extra a decls ->
    annotate a $ ppDecls layout decls

ppDecls :: Layout -> NonEmpty (Decl a) -> Doc (DocAnn a)
ppDecls layout decls =
  let
    -- Whitespace layout cannot represent empty declarations, so we need to
    -- switch to braces.
    layout' =
      if any isEmpty decls then
        Braces
      else
        layout
  in
    ppBlock layout' $
      fmap (ppDecl layout) decls

isEmpty :: Decl a -> Bool
isEmpty = \case
  DeclEmpty _ ->
    True
  _ ->
    False

ppDecl :: Layout -> Decl a -> Doc (DocAnn a)
ppDecl layout = \case
  DeclType a fun cs typ ->
    annotate a $
      defVarId fun <+>
      ppOperator ":" <+>
      hsep (fmap ppConstraint cs <> [ppType typ])
  DeclFun a fun params exp ->
    annotate a $
      defVarId fun <+>
      hsep1 (fmap parVarId params) <+>
      ppOperator "=" <%>
      indent 2 (ppExp layout exp)
  DeclPat a pat exp ->
    annotate a $
      ppPat pat <+>
      ppOperator "=" <%>
      indent 2 (ppExp layout exp)
  DeclEmpty a ->
    annotate a mempty

ppConstraint :: Constraint a -> Doc (DocAnn a)
ppConstraint = \case
  Constraint a ts ->
    annotate a $
      hsep1 (fmap ppTypeSimple ts) <+>
      ppOperator "=>"

ppType :: Type a -> Doc (DocAnn a)
ppType = \case
  Type a ts Nothing ->
    annotate a $
      hsep1 (fmap ppTypeSimple ts)
  Type a ts (Just typ) ->
    annotate a $
      hsep1 (fmap ppTypeSimple ts) <+>
      ppOperator "->" <+>
      ppType typ

ppTypeSimple :: TypeSimple a -> Doc (DocAnn a)
ppTypeSimple = \case
  TypeCon a con ->
    annotate a $
      useConId con
  TypeVar a var ->
    annotate a $
      parVarId var
  TypeTuple a ts ->
    annotate a . ppParens $
      fmap ppType ts

ppExp :: Layout -> Exp a -> Doc (DocAnn a)
ppExp layout = \case
  In a ctx@(Let _ _) exp ->
    annotate a $
      ppContext layout ctx <%>
      ppKeyword "in" <%>
      indent 2 (ppExp layout exp)
  In a ctx exp ->
    annotate a $
      ppContext layout ctx <+> ppKeyword "in" <%>
      ppExp layout exp
  If a gs exp els ->
    annotate a $
      ppKeyword "if" <+> ppGuards gs <+> ppKeyword "then" <%>
      indent 2 (ppExp layout exp) <%>
      ppElse layout els
  Tail a tl ->
    annotate a $
      ppTail layout tl
  App a xs mbop ->
    let
      xs' =
        fmap (ppAtom layout) xs
    in
      annotate a . hsep1 $
        case mbop of
          Nothing ->
            xs'
          Just bop ->
            xs' Semigroup.<>
            if isMultiline (hsep1 xs') then
              pure $ ppTail Braces bop
            else
              pure $ ppTail layout bop

isMultiline :: Doc a -> Bool
isMultiline =
  (> 1) . length . T.lines . renderPlain

ppTail :: Layout -> Tail a -> Doc (DocAnn a)
ppTail layout = \case
  Op a op exp ->
    annotate a $
      ppVarOp op <+> ppExp layout exp
  Of a (alt :| []) ->
    annotate a $
      case layout of
        Spaces ->
          ppKeyword "of" <+> ppAlt layout alt
        Braces ->
          ppKeyword "of" <+> ppDelimiter "{" <+> ppAlt layout alt <%>
          ppDelimiter "}"
  Of a alts ->
    annotate a $
      ppKeyword "of" <%>
      indent 2 (ppBlock layout $ fmap (ppAlt layout) alts)

ppAtom :: Layout -> Atom a -> Doc (DocAnn a)
ppAtom layout = \case
  Lit a lit ->
    annotate a $
      ppLit lit
  Var a var ps ->
    annotate a $
      hcat (useVarId var : fmap usePrjId ps)
  Con a con ->
    annotate a $ useConId con
  Tuple a exps ->
    annotate a . ppParens $
      fmap (ppExp layout) exps

ppContext :: Layout -> Context a -> Doc (DocAnn a)
ppContext layout = \case
  Let a decls ->
    annotate a $
      ppKeyword "let" <%>
      indent 2 (ppDecls layout decls)
  Group a exp ->
    annotate a $
      ppKeyword "group" <+> ppExp layout exp
  Distinct a exp ->
    annotate a $
      ppKeyword "distinct" <+> ppExp layout exp
  Filter a gs ->
    annotate a $
      ppKeyword "filter" <+> ppGuards gs
  Latest a size ->
    annotate a $
      ppKeyword "latest" <+> ppLatestSize size
  Windowed a window ->
    annotate a $
      ppKeyword "windowed" <+> ppWindow window

ppLatestSize :: LatestSize a -> Doc (DocAnn a)
ppLatestSize = \case
  LatestSize a int ->
    annotate a $
      ppInteger int

ppElse :: Layout -> Else a -> Doc (DocAnn a)
ppElse layout = \case
  ElseIf a gs exp els ->
    annotate a $
      ppKeyword "if" <+> ppGuards gs <+> ppKeyword "then" <%>
      indent 2 (ppExp layout exp) <%>
      ppElse layout els
  Else a exp ->
    annotate a $
      ppKeyword "else" <%>
      indent 2 (ppExp layout exp)

ppAlt :: Layout -> Alt a -> Doc (DocAnn a)
ppAlt layout = \case
  Alt a pat rhs ->
    annotate a $
      ppPat pat <> ppAltRhs layout rhs

ppAltRhs :: Layout -> AltRhs a -> Doc (DocAnn a)
ppAltRhs layout = \case
  AltThen a exp ->
    annotate a $
      mempty <+> ppKeyword "then" <%>
      indent 2 (ppExp layout exp)
  AltIf a gs exp Nothing ->
    annotate a $
      mempty <+> align (ppKeyword "if" <+> ppGuards gs <+> ppKeyword "then") <%>
      indent 2 (ppExp layout exp)
  AltIf a gs exp mels ->
    mempty <%> indent 2 (ppAltIf layout a gs exp mels)

ppAltIf :: Layout -> a -> NonEmpty (Guard a) -> Exp a -> Maybe (AltElse a) -> Doc (DocAnn a)
ppAltIf layout a gs exp mels =
  annotate a $
    ppKeyword "if" <+> ppGuards gs <+> ppKeyword "then" <%>
    indent 2 (ppExp layout exp) <>
    case mels of
      Nothing ->
        mempty
      Just els ->
        mempty <%> ppAltElse layout els

ppAltElse :: Layout -> AltElse a -> Doc (DocAnn a)
ppAltElse layout = \case
  AltElseIf a gs exp mels ->
    ppAltIf layout a gs exp mels
  AltElse a exp ->
    annotate a $
      ppKeyword "else" <%>
      indent 2 (ppExp layout exp)

ppGuards :: NonEmpty (Guard a) -> Doc (DocAnn a)
ppGuards =
  ppCommas . fmap (ppGuard Braces)

ppGuard :: Layout -> Guard a -> Doc (DocAnn a)
ppGuard layout = \case
  GuardPat a pat exp ->
    annotate a $
      ppPat pat <+> ppOperator "<-" <+> ppExp layout exp
  GuardExp a exp ->
    annotate a $
      ppExp layout exp

ppPat :: Pat a -> Doc (DocAnn a)
ppPat = \case
  PatNeg a neg ->
    annotate a $
      ppNeg neg
  PatCon a con pats ->
    annotate a . hsep1 $
      useConId con `NonEmpty.cons` fmap ppPat1 pats
  Pat1 a pat ->
    annotate a $
      ppPat1 pat

ppPat1 :: Pat1 a -> Doc (DocAnn a)
ppPat1 = \case
  Pat1Lit a lit ->
    annotate a $
      ppLit lit
  Pat1Con a con ->
    annotate a $
      useConId con
  Pat1Var a var ->
    annotate a $
      defVarId var
  Pat1Wild a ->
    annotate a $
      ppDelimiter "_"
  Pat1Tuple a ps ->
    annotate a . ppParens $
      fmap ppPat ps

ppWindow :: Window a -> Doc (DocAnn a)
ppWindow = \case
  Fixed a x xu ->
    annotate a $
      ppWindowSize x <+> ppWindowUnit xu
  Between a x xu y yu ->
    annotate a $
      ppTemporal "between" <+>
      ppWindowSize x <+> ppWindowUnit xu <+>
      ppTemporal "and" <+>
      ppWindowSize y <+> ppWindowUnit yu

ppWindowSize :: WindowSize a -> Doc (DocAnn a)
ppWindowSize = \case
  WindowSize a n ->
    annotate a $
      ppInteger n

ppWindowUnit :: WindowUnit a -> Doc (DocAnn a)
ppWindowUnit = \case
  Days a ->
    annotate a $
      ppTemporal "days"
  Months a ->
    annotate a $
      ppTemporal "months"
  Weeks a ->
    annotate a $
      ppTemporal "weeks"

ppNeg :: Neg a -> Doc (DocAnn a)
ppNeg = \case
  NegInteger a integer ->
    annotate a $
      ppOperator "-" <> ppInteger integer
  NegRational a rational ->
    annotate a $
      ppOperator "-" <> ppRational rational

ppLit :: Lit a -> Doc (DocAnn a)
ppLit = \case
  LitInteger a integer ->
    annotate a $
      ppInteger integer
  LitRational a rational ->
    annotate a $
      ppRational rational
  LitString a string ->
    annotate a $
      ppString string
  LitDate a date ->
    annotate a $
      ppDate date

useVarId :: VarId a -> Doc (DocAnn a)
useVarId = \case
  VarId _ "case" ->
    ppKeyword "case"
  VarId _ "fold" ->
    ppKeyword "fold"
  VarId _ "fold1" ->
    ppKeyword "fold1"
  VarId _ "for" ->
    ppKeyword "for"
  vid ->
    Leijen.annotate DocUseVar $ ppVarId vid

defVarId :: VarId a -> Doc (DocAnn a)
defVarId =
  Leijen.annotate DocDefVar . ppVarId

parVarId :: VarId a -> Doc (DocAnn a)
parVarId =
  Leijen.annotate DocParVar . ppVarId

useConId :: ConId a -> Doc (DocAnn a)
useConId =
  Leijen.annotate DocUseCon . ppConId

usePrjId :: PrjId a -> Doc (DocAnn a)
usePrjId = \case
  PrjId a prj ->
    annotate a $
      Leijen.annotate DocUseVar (ppText "." <> ppText prj)

ppVarId :: VarId a -> Doc (DocAnn a)
ppVarId = \case
  VarId a var ->
    annotate a $ ppText var

ppVarOp :: VarOp a -> Doc (DocAnn a)
ppVarOp = \case
  VarOp a op ->
    annotate a $ ppOperator op

ppConId :: ConId a -> Doc (DocAnn a)
ppConId = \case
  ConId a con ->
    annotate a $ ppText con

ppKeyword :: Text -> Doc (DocAnn a)
ppKeyword =
  Leijen.annotate DocKeyword . ppText

ppTemporal :: Text -> Doc (DocAnn a)
ppTemporal =
  Leijen.annotate DocTemporal . ppText

ppDelimiter :: Text -> Doc (DocAnn a)
ppDelimiter =
  Leijen.annotate DocDelimiter . ppText

ppOperator :: Text -> Doc (DocAnn a)
ppOperator =
  Leijen.annotate DocOperator . ppText

ppInteger :: Integer -> Doc (DocAnn a)
ppInteger =
  Leijen.annotate DocLiteral . Leijen.integer

ppRational :: Scientific -> Doc (DocAnn a)
ppRational =
  Leijen.annotate DocLiteral . Leijen.text . show

ppString :: Text -> Doc (DocAnn a)
ppString txt =
  Leijen.annotate DocLiteral $
    Leijen.char '"' <>
    Leijen.text (escapeChars $ T.unpack txt) <>
    Leijen.char '"'

ppDate :: Day -> Doc (DocAnn a)
ppDate =
  Leijen.annotate DocLiteral . Leijen.text . showGregorian

ppText :: Text -> Doc a
ppText =
  Leijen.text . T.unpack

annotate :: a -> Doc (DocAnn a) -> Doc (DocAnn a)
annotate a =
  Leijen.annotate (DocAnn a)

hsep1 :: NonEmpty (Doc a) -> Doc a
hsep1 =
  hsep . NonEmpty.toList

ppCommas :: NonEmpty (Doc (DocAnn a)) -> Doc (DocAnn a)
ppCommas =
  let
    sep = ppDelimiter "," <+> mempty
  in
    hcat .
    List.zipWith (<>) (mempty : List.repeat sep) .
    NonEmpty.toList

ppParens :: [Doc (DocAnn a)] -> Doc (DocAnn a)
ppParens =
  let
    bra = ppDelimiter "("
    ket = ppDelimiter ")"
    sep = ppDelimiter "," <+> mempty
  in
    \case
      [] ->
        hcat [bra, ket]
      ds ->
        hcat $
          List.zipWith (<>) (bra : List.repeat sep) (fmap align ds) <> [ket]

ppBlock :: Layout -> NonEmpty (Doc (DocAnn a)) -> Doc (DocAnn a)
ppBlock layout ds =
  case layout of
    Spaces ->
      align . vsep . fmap align $ toList ds
    Braces ->
      ppBraces ds

ppBraces :: NonEmpty (Doc (DocAnn a)) -> Doc (DocAnn a)
ppBraces ds =
  let
    bra = ppDelimiter "{"
    ket = ppDelimiter "}"
    sep = ppDelimiter ";"
  in
    align . vsep $
      List.zipWith
        (<+>)
        (bra : List.repeat sep)
        (fmap align $ toList ds) <> [ket]
