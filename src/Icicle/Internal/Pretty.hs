-- | Provide a *slightly* better interface to pretty printer.
--
-- Problem is that it defines (<>), but so does Data.Monoid.
--
-- Its (<>) really is a monoid though, so hiding it and adding
-- an orphan instance that does the same thing seems fine to me.
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Internal.Pretty (
    module PP
    ) where

-- The one we want to export without <>
import              Text.PrettyPrint.Leijen as PP hiding ((<>))
-- The one with <>
import              Text.PrettyPrint.Leijen as PJOIN

import              P

instance Monoid Doc where
 mempty  =  PJOIN.empty
 mappend = (PJOIN.<>)

