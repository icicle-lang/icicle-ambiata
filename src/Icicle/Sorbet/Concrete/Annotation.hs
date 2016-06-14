{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Sorbet.Concrete.Annotation (
    annotOfRepl
  , annotOfExtra
  , annotOfDecl
  , annotOfConstraint
  , annotOfType
  , annotOfTypeSimple
  , annotOfExp
  , annotOfTail
  , annotOfAtom
  , annotOfContext
  , annotOfLatestSize
  , annotOfElse
  , annotOfAlt
  , annotOfAltRhs
  , annotOfAltElse
  , annotOfGuard
  , annotOfPat
  , annotOfPat1
  , annotOfWindow
  , annotOfWindowSize
  , annotOfWindowUnit
  , annotOfNeg
  , annotOfLit
  , annotOfPrjId
  , annotOfVarId
  , annotOfVarOp
  , annotOfConId
  ) where

import           Icicle.Sorbet.Concrete.Syntax


annotOfRepl :: Repl a -> a
annotOfRepl = \case
  From a _ _ ->
    a

annotOfExtra :: Extra a -> a
annotOfExtra = \case
  Extra a _ ->
    a

annotOfDecl :: Decl a -> a
annotOfDecl = \case
  DeclType a _ _ _ ->
    a
  DeclFun a _ _ _ ->
    a
  DeclPat a _ _ ->
    a
  DeclEmpty a ->
    a

annotOfConstraint :: Constraint a -> a
annotOfConstraint = \case
  Constraint a _ ->
    a

annotOfType :: Type a -> a
annotOfType = \case
  Type a _ _ ->
    a

annotOfTypeSimple :: TypeSimple a -> a
annotOfTypeSimple = \case
  TypeCon a _ ->
    a
  TypeVar a _ ->
    a
  TypeTuple a _ ->
    a

annotOfExp :: Exp a -> a
annotOfExp = \case
  In a _ _ ->
    a
  If a _ _ _ ->
    a
  App a _ _ ->
    a
  Tail a _ ->
    a

annotOfTail :: Tail a -> a
annotOfTail = \case
  Op a _ _ ->
    a
  Of a _ ->
    a

annotOfAtom :: Atom a -> a
annotOfAtom = \case
  Lit a _ ->
    a
  Var a _ _ ->
    a
  Con a _ ->
    a
  Tuple a _ ->
    a

annotOfContext :: Context a -> a
annotOfContext = \case
  Let a _ ->
    a
  Windowed a _ ->
    a
  Group a _ ->
    a
  Distinct a _ ->
    a
  Filter a _ ->
    a
  Latest a _ ->
    a

annotOfLatestSize :: LatestSize a -> a
annotOfLatestSize = \case
  LatestSize a _ ->
    a

annotOfElse :: Else a -> a
annotOfElse = \case
  ElseIf a _ _ _ ->
    a
  Else a _ ->
    a

annotOfAlt :: Alt a -> a
annotOfAlt = \case
  Alt a _ _ ->
    a

annotOfAltRhs :: AltRhs a -> a
annotOfAltRhs = \case
  AltThen a _ ->
    a
  AltIf a _ _ _ ->
    a

annotOfAltElse :: AltElse a -> a
annotOfAltElse = \case
  AltElseIf a _ _ _ ->
    a
  AltElse a _ ->
    a

annotOfGuard :: Guard a -> a
annotOfGuard = \case
  GuardPat a _ _ ->
    a
  GuardExp a _ ->
    a

annotOfPat :: Pat a -> a
annotOfPat = \case
  PatNeg a _ ->
    a
  PatCon a _ _ ->
    a
  Pat1 a _ ->
    a

annotOfPat1 :: Pat1 a -> a
annotOfPat1 = \case
  Pat1Lit a _ ->
    a
  Pat1Con a _ ->
    a
  Pat1Var a _ ->
    a
  Pat1Wild a ->
    a
  Pat1Tuple a _ ->
    a

annotOfWindow :: Window a -> a
annotOfWindow = \case
  Fixed a _ _ ->
    a
  Between a _ _ _ _ ->
    a

annotOfWindowSize :: WindowSize a -> a
annotOfWindowSize = \case
  WindowSize a _ ->
    a

annotOfWindowUnit :: WindowUnit a -> a
annotOfWindowUnit = \case
  Days a ->
    a
  Months a ->
    a
  Weeks a ->
    a

annotOfNeg :: Neg a -> a
annotOfNeg = \case
  NegInteger a _ ->
    a
  NegRational a _ ->
    a

annotOfLit :: Lit a -> a
annotOfLit = \case
  LitInteger a _ ->
    a
  LitRational a _ ->
    a
  LitString a _ ->
    a
  LitDate a _ ->
    a

annotOfPrjId :: PrjId a -> a
annotOfPrjId = \case
  PrjId a _ ->
    a

annotOfVarId :: VarId a -> a
annotOfVarId = \case
  VarId a _ ->
    a

annotOfVarOp :: VarOp a -> a
annotOfVarOp = \case
  VarOp a _ ->
    a

annotOfConId :: ConId a -> a
annotOfConId = \case
  ConId a _ ->
    a
