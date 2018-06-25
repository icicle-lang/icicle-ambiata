{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Sorbet.Concrete.Syntax (
    Repl(..)
  , Extra(..)
  , Decl(..)
  , Constraint(..)
  , Type(..)
  , TypeSimple(..)
  , Exp(..)
  , Tail(..)
  , Atom(..)
  , Context(..)
  , LatestSize(..)
  , Else(..)
  , Alt(..)
  , AltRhs(..)
  , AltElse(..)
  , Guard(..)
  , Pat(..)
  , Pat1(..)
  , Window(..)
  , WindowSize(..)
  , WindowUnit(..)
  , Neg(..)
  , Lit(..)
  , PrjId(..)
  , VarId(..)
  , VarOp(..)
  , ConId(..)
  ) where

import           Data.Data (Data)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Scientific (Scientific)
import           Data.Thyme (Day)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           P hiding (Alt)


data Repl a =
    From !a !(VarId a) !(Exp a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Extra a =
    Extra !a !(NonEmpty (Decl a))
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Decl a =
    DeclType !a !(VarId a) ![Constraint a] !(Type a)
  | DeclFun !a !(VarId a) !(NonEmpty (VarId a)) !(Exp a)
  | DeclPat !a !(Pat a) !(Exp a)
  | DeclEmpty !a
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Constraint a =
    Constraint !a !(NonEmpty (TypeSimple a))
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Type a =
    Type !a !(NonEmpty (TypeSimple a)) !(Maybe (Type a))
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data TypeSimple a =
    TypeCon !a !(ConId a)
  | TypeVar !a !(VarId a)
  | TypeTuple !a ![Type a]
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Exp a =
    In !a !(Context a) !(Exp a)
  | If !a !(NonEmpty (Guard a)) !(Exp a) !(Else a)
  | App !a !(NonEmpty (Atom a)) !(Maybe (Tail a))
  | Tail !a !(Tail a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Tail a =
    Op !a !(VarOp a) !(Exp a)
  | Of !a !(NonEmpty (Alt a))
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Atom a =
    Lit !a !(Lit a)
  | Var !a !(VarId a) ![PrjId a]
  | Con !a !(ConId a)
  | Tuple !a ![Exp a]
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Context a =
    Let !a !(NonEmpty (Decl a))
  | Group !a !(Exp a)
  | Distinct !a !(Exp a)
  | Filter !a !(NonEmpty (Guard a))
  | Latest !a !(LatestSize a)
  | Windowed !a !(Window a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data LatestSize a =
    LatestSize !a !Integer
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Else a =
    ElseIf !a !(NonEmpty (Guard a)) !(Exp a) !(Else a)
  | Else !a !(Exp a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Alt a =
    Alt !a !(Pat a) !(AltRhs a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data AltRhs a =
    AltThen !a !(Exp a)
  | AltIf !a !(NonEmpty (Guard a)) !(Exp a) !(Maybe (AltElse a))
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data AltElse a =
    AltElseIf !a !(NonEmpty (Guard a)) !(Exp a) !(Maybe (AltElse a))
  | AltElse !a !(Exp a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Guard a =
    GuardPat !a !(Pat a) !(Exp a)
  | GuardExp !a !(Exp a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Pat a =
    PatNeg !a !(Neg a)
  | PatCon !a !(ConId a) !(NonEmpty (Pat1 a))
  | Pat1 !a !(Pat1 a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Pat1 a =
    Pat1Lit !a !(Lit a)
  | Pat1Con !a !(ConId a)
  | Pat1Var !a !(VarId a)
  | Pat1Wild !a
  | Pat1Tuple !a ![Pat a]
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Window a =
    Fixed !a !(WindowSize a) !(WindowUnit a)
  | Between !a !(WindowSize a) !(WindowUnit a) !(WindowSize a) !(WindowUnit a)
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data WindowSize a =
    WindowSize !a !Integer
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data WindowUnit a =
    Days !a
  | Months !a
  | Weeks !a
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Neg a =
    NegInteger !a !Integer
  | NegRational !a !Scientific
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data Lit a =
    LitInteger !a !Integer
  | LitRational !a !Scientific
  | LitString !a !Text
  | LitDate !a !Day
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data PrjId a =
    PrjId !a !Text
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data VarId a =
    VarId !a !Text
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data VarOp a =
    VarOp !a !Text
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)

data ConId a =
    ConId !a !Text
    deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor, Foldable, Traversable)
