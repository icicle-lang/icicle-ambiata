{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Function (
    Function       (..)
  , reannotF
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

reannotF :: (a -> a') -> Function a n -> Function a' n
reannotF f fun
 = fun { arguments = fmap (first f) (arguments fun)
       , body = reannotQ f (body fun) }

instance Pretty n => Pretty (Function a n) where
 pretty q =
  let p = (pretty . snd) <$> (arguments q)
  in (sep p) <> line <> "=" <+> pretty (body q)

