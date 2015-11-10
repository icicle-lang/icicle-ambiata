{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Struct (
    makeStructDeclaration
  , makeStructDeclarations
  ) where

import           Icicle.Common.Type

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Type

import           P

import qualified Data.Map as Map

import           Data.Set (Set)
import qualified Data.Set as Set

makeStructDeclarations :: Set ValType -> Doc
makeStructDeclarations tys
 = mconcat
 $ fmap go
 $ Set.toList tys
 where
  go (StructT st)
   = makeStructDeclaration st <> line
  go _
   = ""

makeStructDeclaration :: StructType -> Doc
makeStructDeclaration st@(StructType fs)
 = vsep
 [ "struct " <> typename
 , "{"
 , indent 4
 $ vsep 
 ( (fmap goField $ Map.toList fs)
   <> [ctorNil, ctorPack])
 , "};" ]
 where
  typename = noPadSeaOfValType (StructT st)

  goField (nm,ty)
   = seaOfValType ty <+> seaOfName nm <> ";"
   <> line
   <> seaOfValType ty <+> "getField__" <> seaOfName nm <> "() const"
   <> line
   <> "{ return this->" <> seaOfName nm <> "; }"

  ctorNil
   = typename <> "() {}"

  ctorPack
   = typename <> tupled (fmap arg $ Map.toList fs)
   <> line
   <> "{"
   <> indent 4 (vsep $ fmap set $ Map.toList fs)
   <> "}"

  arg (nm,ty)
   = noPadSeaOfValType ty <+> seaOfName nm

  set (nm,_)
   = "this->" <> seaOfName nm <> " = " <> seaOfName nm <> ";"

