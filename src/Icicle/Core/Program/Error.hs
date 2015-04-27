-- A whole core program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Program.Error (
      ProgramError (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Exp
import              Icicle.Core.Stream
import              Icicle.Core.Reduce

import              P

data ProgramError n
 = ProgramErrorPre      (ExpError    n)
 | ProgramErrorStream   (StreamError n)
 | ProgramErrorReduce   (ReduceError n)
 | ProgramErrorPost     (ExpError    n)
 | ProgramErrorReturn   (ExpError    n)
 | ProgramErrorNameNotUnique (Name n)

instance (Pretty n) => Pretty (ProgramError n) where
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
  where
   ind d
    = line <> indent 4 d

