{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Function (
    Function       (..)
  ) where

import                  Icicle.Internal.Pretty
import                  Icicle.Source.Query.Query
import                  Icicle.Common.Base

import                  P

data Function a n
  = Function
  { arguments :: [(a,Name n)]
  , body      :: Query a n }
  deriving (Show, Eq)

instance Pretty n => Pretty (Function a n) where
 pretty q =
  let p = (pretty . snd) <$> (arguments q)
  in (sep p) <> line <> "=" <+> pretty (body q)

