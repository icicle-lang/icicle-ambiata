-- | Typechecking errors for a core program
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Error (
      ProgramError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp
import              Icicle.Core.Stream
import              Icicle.Core.Reduce

import              P


-- | Possible errors, mainly deferred to component
data ProgramError a n
 = ProgramErrorPre      (ExpError    a n Prim)
 | ProgramErrorStream   (StreamError a n)
 | ProgramErrorReduce   (ReduceError a n)
 | ProgramErrorPost     (ExpError    a n Prim)
 | ProgramErrorReturn   (ExpError    a n Prim)
 | ProgramErrorNameNotUnique (Name n)
 | ProgramErrorReturnNotValueType Type
 deriving Show


instance (Pretty n) => Pretty (ProgramError a n) where
 pretty e
  = case e of
    ProgramErrorPre    err
     -> text "Pre error: " <> ind (pretty err)
    ProgramErrorStream err
     -> text "Stream error: " <> ind (pretty err)
    ProgramErrorReduce err
     -> text "Reduce error: " <> ind (pretty err)
    ProgramErrorPost err
     -> text "Post error: " <> ind (pretty err)
    ProgramErrorReturn err
     -> text "Return error: " <> ind (pretty err)
    ProgramErrorNameNotUnique n
     -> text "Name not unique: " <> pretty n
    ProgramErrorReturnNotValueType t
     ->  text "The return expression has type " <> pretty t <> text " but should be a value type."

  where
   ind d
    = line <> indent 4 d

