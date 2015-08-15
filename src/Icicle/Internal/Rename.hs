-- | Renaming all the different ASTs in Icicle.
--
module Icicle.Internal.Rename where

import           Icicle.Common.Base
import           Icicle.Common.Exp
import qualified Icicle.Core.Exp             as CE
import qualified Icicle.Core.Program.Program as CP
import qualified Icicle.Core.Reduce          as CR
import qualified Icicle.Core.Stream          as CS
import qualified Icicle.Source.Query         as SQ
import qualified Icicle.Source.Query.Exp     as SE

renameQT :: (n -> m) -> SQ.QueryTop a n -> SQ.QueryTop a m
renameQT f (SQ.QueryTop x q)
  = SQ.QueryTop (renameN f x) (renameQ f q)

renameQ :: (n -> m) -> SQ.Query a n -> SQ.Query a m
renameQ f (SQ.Query cs x)
  = SQ.Query (map (renameC f) cs) (renameX f x)

renameC :: (n -> m) -> SQ.Context a n -> SQ.Context a m
renameC f cc = case cc of
  SQ.Windowed a b c -> SQ.Windowed a b c
  SQ.Latest   a b   -> SQ.Latest   a b
  SQ.GroupBy  a e   -> SQ.GroupBy  a (renameX f e)
  SQ.Distinct a e   -> SQ.Distinct a (renameX f e)
  SQ.Filter   a e   -> SQ.Filter   a (renameX f e)
  SQ.LetFold  a x   -> SQ.LetFold  a (renameF f x)
  SQ.Let      a n e -> SQ.Let      a (renameN f n) (renameX f e)

renameF :: (n -> m) -> SQ.Fold (SQ.Query a n) a n -> SQ.Fold (SQ.Query a m) a m
renameF f d
  = SQ.Fold
    (renameN f $ SQ.foldBind d)
    (renameX f $ SQ.foldInit d)
    (renameX f $ SQ.foldWork d)
    (SQ.foldType d)

renameX :: (n -> m) -> SQ.Exp a n -> SQ.Exp a m
renameX f e = case e of
  SE.Var a n     -> SE.Var a (renameN f n)
  SE.Nested a q  -> SE.Nested a (renameQ f q)
  SE.App a e1 e2 -> SE.App a (renameX f e1) (renameX f e2)
  SE.Prim a p    -> SE.Prim a p

renameN :: (n -> m) -> Name n -> Name m
renameN f (Name v)        = Name    (f v)
renameN f (NameMod n1 n2) = NameMod (f n1) (renameN f n2)

renameP :: (n -> m) -> CP.Program n -> CP.Program m
renameP f prog
  = CP.Program
    { CP.input     = CP.input prog
    , CP.precomps  = map (renameNX f) (CP.precomps prog)
    , CP.streams   = map (renameNS f) (CP.streams prog)
    , CP.reduces   = map (renameNR f) (CP.reduces prog)
    , CP.postdate  = fmap (renameN f) (CP.postdate prog)
    , CP.postcomps = map (renameNX f) (CP.postcomps prog)
    , CP.returns   = renameCX f (CP.returns prog) }

renameNS :: (n -> m) -> (Name n, CS.Stream n) -> (Name m, CS.Stream m)
renameNS f (n, s) = (renameN f n, renameS f s)

renameS :: (n -> m) -> CS.Stream n -> CS.Stream m
renameS _ CS.Source                 = CS.Source
renameS f (CS.SWindow t x mx n)     = CS.SWindow t (renameCX f x) (fmap (renameCX f) mx) (renameN f n)
renameS f (CS.STrans i e n)         = CS.STrans i (renameCX f e) (renameN f n)

renameNR :: (n -> m) -> (Name n, CR.Reduce n) -> (Name m, CR.Reduce m)
renameNR f (n, r) = (renameN f n, renameR f r)

renameR :: (n -> m) -> CR.Reduce n -> CR.Reduce m
renameR f = CR.renameReduce (fmap f)

renameNX :: (n -> m) -> (Name n, CE.Exp n) -> (Name m, CE.Exp m)
renameNX f (n, e) = (renameN f n, renameCX f e)

renameCX :: (n -> m) -> CE.Exp n -> CE.Exp m
renameCX f e = case e of
  XVar n       -> XVar (renameN f n)
  XApp e1 e2   -> XApp (renameCX f e1) (renameCX f e2)
  XLam n t  e1 -> XLam (renameN f n) t (renameCX f e1)
  XLet n e1 e2 -> XLet (renameN f n) (renameCX f e1) (renameCX f e2)
  XPrim p      -> XPrim p
  XValue x y   -> XValue x y

