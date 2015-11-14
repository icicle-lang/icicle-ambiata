{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Stream.Stream (
      Stream          (..)
    , StreamTransform (..)
    , typeOfStreamTransform
    , inputOfStreamTransform
    , outputOfStreamTransform
    , renameStream
    , isStreamWindowed
    , inputOfStream
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp
import              Icicle.Common.Exp.Exp (renameExp)

import              P
import qualified    Data.List   as List



data Stream a n
 = Source
 | SWindow ValType WindowUnit (Maybe WindowUnit) WindowFrame (Name n)
 | STrans StreamTransform (Exp a n) (Name n)
 deriving (Eq,Ord,Show)

-- | Explicitly carrying around the type parameters is annoying, but makes typechecking simpler
data StreamTransform
 = SFilter ValType
 | SMap    ValType ValType
 deriving (Eq,Ord,Show)

typeOfStreamTransform :: StreamTransform -> Type
typeOfStreamTransform st
 = case st of
    SFilter t -> FunT [funOfVal t] BoolT
    SMap  p q -> FunT [funOfVal p] q

inputOfStreamTransform :: StreamTransform -> ValType
inputOfStreamTransform st
 = case st of
    SFilter t -> t
    SMap  p _ -> p


outputOfStreamTransform :: StreamTransform -> ValType
outputOfStreamTransform st
 = case st of
    SFilter t -> t
    SMap  _ q -> q


renameStream :: (Name n -> Name n') -> Stream a n -> Stream a n'
renameStream _ Source                 = Source
renameStream f (SWindow t x mx fr n)  = SWindow t x mx fr (f n)
renameStream f (STrans t x n)         = STrans t (renameExp f x) (f n)


-- | Check if given stream originates from a windowed or not
isStreamWindowed :: Eq n => [(Name n, Stream a n)] -> Name n -> Bool
isStreamWindowed ss nm
 = case List.lookup nm ss of
    Just Source                 -> False
    Just SWindow{}              -> True
    Just (STrans _ _ inp)       -> isStreamWindowed ss inp

    Nothing -> False -- error...


-- | Get name of input stream, if applicable
inputOfStream :: Stream a n -> Maybe (Name n)
inputOfStream  Source                = Nothing
inputOfStream (SWindow _ _ _ _ inp)  = Just inp
inputOfStream (STrans  _ _   inp)    = Just inp


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Stream a n) where
 pretty Source         = text "source"
 pretty (SWindow t x mx fr inp)
                       = text "swindow [" <> pretty t <> text "]"
                       </> parens (pretty x) </> parens (pretty mx) </> parens (pretty fr) </> pretty inp

 pretty (STrans t x n) = pretty t </> parens (pretty x) </> pretty n

instance Pretty StreamTransform where
 pretty (SFilter t) = text "sfilter [" <> pretty t <> text "]"
 pretty (SMap p q)  = text "smap    [" <> pretty p <> text "] [" <> pretty q <> text "]"

