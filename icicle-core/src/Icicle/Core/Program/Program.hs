-- | An entire core program
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Program.Program (
       Program (..)
     , renameProgram
     ) where

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.Exp.Exp (renameExp)
import           Icicle.Common.Type
import           Icicle.Core.Exp
import           Icicle.Core.Stream.Stream
import           Icicle.Data.Name
import           Icicle.Internal.Pretty

import           P


-- | Core program composed of different stages of bindings
data Program a n =
 Program {
 -- | The type of the input/concrete feature
   inputType    :: !ValType
 , factValName  :: !(Name n)
 , factTimeName :: !(Name n)
 , snaptimeName :: !(Name n)
 , maxMapSize   :: !(Name n)

 -- | All precomputations, made before starting to read from feature source
 , precomps     :: ![(Name n, Exp a n)]

 -- | Stream things
 , streams      :: ![Stream a n]

 -- | Postcomputations with access to last value of all streams
 , postcomps    :: ![(Name n, Exp a n)]

 -- | The return values
 , returns      :: ![(OutputId, Exp a n)]
 }
 deriving (Show, Eq, Ord, Generic)

instance (NFData a, NFData n) => NFData (Program a n)

renameProgram :: (Name n -> Name n') -> Program a n -> Program a n'
renameProgram f p
  = p
  { factValName  = f $ factValName  p
  , factTimeName = f $ factTimeName p
  , snaptimeName = f $ snaptimeName p
  , maxMapSize   = f $ maxMapSize   p
  , precomps       = binds  renameExp      (precomps   p)
  , streams        = fmap  (renameStream f)(streams    p)
  , postcomps      = binds  renameExp      (postcomps  p)
  -- Now, we actually do not want to modify the names of the outputs.
  -- They should stay the same over the entire life of the program.
  , returns        = fmap (\(a,b) -> (a, renameExp f b)) (returns    p)
  }
  where
   binds r = fmap (\(a,b) -> (f a, r f b))


-- Pretty printing -------------

instance Pretty n => Pretty (Program a n) where
  pretty p =
    let
      ppInput nm ty =
        prettyTypedFlat (annotate AnnBinding $ pretty nm) ty

      ppBind (nm, bind') =
        vsep [
            annotate AnnBinding (pretty nm) <+>
              annotate AnnPunctuation "="
          , indent 2 (pretty bind')
          ]

      ppBinds :: (Pretty a, Pretty b) => [(a, b)] -> Doc
      ppBinds = \case
        [] ->
          annotate AnnPunctuation "<none>"
        xs ->
          vsep $ fmap ppBind xs
    in
      vsep [
          prettyH2 "Inputs"
        , mempty
        , vsep [
              ppInput (factValName p) $
                align (pretty (inputType p))
            , ppInput (factTimeName p) $
                annotate AnnConstructor "Time"
            , ppInput (snaptimeName p) $
                annotate AnnConstructor "SNAPSHOT_TIME"
            , ppInput (maxMapSize p) $
                annotate AnnConstructor "MaxMapSize"
            ]
        , mempty
        , prettyH2 "Precomputations"
        , mempty
        , ppBinds (precomps p)
        , mempty
        , prettyH2 "Streams"
        , mempty
        , vcat (fmap pretty (streams p))
        , mempty
        , prettyH2 "Postcomputations"
        , mempty
        , ppBinds (postcomps p)
        , mempty
        , prettyH2 "Returning"
        , mempty
        , ppBinds (returns p)
        ]
