{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Sea.FromCanyon.Program (
    seaOfPrograms
  , seaOfXValue
  , nameOfCluster
  , nameOfCluster'
  , nameOfKernel
  , nameOfKernel'
  , nameOfClusterState
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Canyon.Program
import           Icicle.Canyon.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Data

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error
import           Icicle.Sea.FromCanyon.Base
import           Icicle.Sea.FromCanyon.Prim
import           Icicle.Sea.FromCanyon.State
import           Icicle.Sea.FromCanyon.Type
import           Icicle.Sea.Name

import           P hiding (head)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ( NonEmpty(..) )


------------------------------------------------------------------------

seaOfPrograms ::
     (Show a, Show n, Pretty n, Eq n)
  => ClusterId
  -> InputId
  -> NonEmpty (Program (Annot a) n)
  -> Either SeaError Doc
seaOfPrograms cid iid programs = do
  cluster <- clusterOfPrograms cid iid programs
  let kernels = NonEmpty.zipWith (seaOfKernel cluster) (clusterKernels cluster) programs
  pure . vsep $
    seaOfClusterState cluster : NonEmpty.toList kernels

seaOfKernel ::
     (Show a, Show n, Pretty n, Eq n)
  => Cluster c k
  -> Kernel k
  -> Program (Annot a) n
  -> Doc
seaOfKernel cluster kernel program =
  let
    time_name =
      mangle . bindtime $ program

    max_heap_name =
      mangle . maxMapSize  $ program
  in
    vsep [
        ""
      , "#line 1 \"kernel function" <+> seaOfClusterInfo cluster <+> pretty (nameOfKernel kernel) <> "\""
      , "void " <> pretty (nameOfKernel kernel) <> "(" <> pretty (nameOfClusterState cluster) <+> "*s)"
      , "{"
      , indent 4 $
          assign (defOfVar' 1 "anemone_mempool_t" "mempool") "s->mempool;"

      , indent 4 $
          assign (defOfVar  0 TimeT $ prettySeaName time_name) ("s->" <> clusterInputTime cluster) <> ";"

      , indent 4 $
          assign (defOfVar  0 IntT $ prettySeaName max_heap_name) "s->max_map_size;"

      , ""
      , indent 4 (seaOfStatement cluster kernel (statements program))
      , "}"
      ]

------------------------------------------------------------------------

seaOfStatement ::
     (Show a, Show n, Pretty n, Eq n)
  => Cluster c k
  -> Kernel k
  -> Statement (Annot a) n
  -> Doc
seaOfStatement cluster kernel stmt
 = case stmt of
     Block []
      -> mempty

     Block (s:[])
      -> seaOfStatement cluster kernel s

     Block (s:ss)
      -> seaOfStatement cluster kernel s <> line
      <> seaOfStatement cluster kernel (Block ss)

     Let name xt xx stmt'
      | n <- mangle name
      -> assign (defOfVar 0 xt $ prettySeaName n) (seaOfExp xx) <> semi <> suffix "let" <> line
      <> seaOfStatement cluster kernel stmt'

     If ii tt (Block [])
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement cluster kernel tt)
              , "}"
              , ""
              ]

     If ii tt ee
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement cluster kernel tt)
              , "} else {"
              , indent 4 (seaOfStatement cluster kernel ee)
              , "}"
              , ""
              ]

     While t name _ end stmt'
      | n <- mangle name
      -> vsep [ "while (" <> prettySeaName n <+> seaOfWhileType t <+> seaOfExp end <> ") {"
              , indent 4 $ seaOfStatement cluster kernel stmt'
              , "}"
              ]
     ForeachInts t name start end stmt'
      | n <- mangle name
      -> vsep [ "for (iint_t" <+> prettySeaName n <+> "=" <+> seaOfExp start <> ";"
                              <+> prettySeaName n <+> seaOfForeachCompare t  <+> seaOfExp end <> ";"
                              <+> prettySeaName n <>  seaOfForeachStep t     <> ") {"
              , indent 4 $ seaOfStatement cluster kernel stmt'
              , "}"
              ]

     ForeachFacts (FactBinds ntime ns) _ stmt'
      | inputStruct <- clusterInputVars cluster
      , ntime' <- mangle ntime
      , ns' <- fmap (first mangle) ns
      -> let
           structAssign :: (SeaName, ValType) -> Doc
           structAssign (n, t)
             | ndoc <- prettySeaName n =
                 assign
                   (defOfVar' 1 ("const" <+> seaOfValType t) ("const" <+> pretty newPrefix <> ndoc))
                   (stNew n) <> semi

           loopAssign :: (SeaName, ValType) -> (SeaName, a) -> Doc
           loopAssign (bindn, t) (inputn,_) =
             assign (defOfVar 0 t $ prettySeaName bindn)
                    (pretty newPrefix <> prettySeaName inputn <> "[i]") <> semi

           factTime = case reverse inputStruct of
                       [] -> seaError "seaOfStatement: no facts" ()
                       ((n,_):_) -> pretty newPrefix <> prettySeaName n <> "[i]"

         in vsep $
            [ ""
            , assign (defOfVar' 0 ("const" <+> seaOfValType IntT) "new_count") (stCount <> ";")
            ] <> fmap structAssign inputStruct <>
            [ ""
            , "for (iint_t i = 0; i < new_count; i++) {"
            , indent 4 $
                assign (defOfVar 0 TimeT $ prettySeaName ntime') factTime <> semi
            , indent 4 $
                vsep (List.zipWith loopAssign ns' inputStruct) <> line <> seaOfStatement cluster kernel stmt'
            , "}"
            , ""
            ]

     Write name xx
      | n <- mangle name
      -> assign (prettySeaName n) (seaOfExp xx) <> semi <> suffix "write"

     LoadResumable name _
      | n <- mangle name
      , nd <- prettySeaName n
      -> vsep [ ""
              , "if (" <> stHas n <> ") {"
              , indent 4 $ assign nd (stRes n) <> semi <> suffix "load"
              , "}" ]

     SaveResumable name _
      | n <- mangle name
      , nd <- prettySeaName n
      -> assign (stHas n) "itrue"       <> semi <> suffix "save" <> line
      <> assign (stRes n) nd <> semi <> suffix "save" <> line

     Output n _ xts
      | ixAssign <- \ix xx -> assign ("s->" <> (prettySeaName $ mangleIx n ix)) (seaOfExp xx) <> semi <> suffix "output"
      -> vsep (List.zipWith ixAssign [0..] (fmap fst xts))
  where
   stNew   n = "s->" <> clusterInputNew (prettySeaName n)
   stRes   n = "s->" <> clusterInputRes (nameOfResumable kernel $ prettySeaName n)
   stHas   n = "s->" <> clusterInputHas (nameOfResumable kernel $ prettySeaName n)
   stCount   = "s->" <> clusterNewCount


seaOfForeachCompare :: ForeachType -> Doc
seaOfForeachCompare ForeachStepUp   = "<"
seaOfForeachCompare ForeachStepDown = ">"

seaOfForeachStep :: ForeachType -> Doc
seaOfForeachStep ForeachStepUp   = "++"
seaOfForeachStep ForeachStepDown = "--"

seaOfWhileType :: WhileType -> Doc
seaOfWhileType WhileEq = "=="
seaOfWhileType WhileNe = "!="

------------------------------------------------------------------------

seaOfExp :: (Show a, Show n, Pretty n, Eq n)
         => Exp (Annot a) n Prim -> Doc
seaOfExp xx
 = case xx of
     XValue _ t v
      -> seaOfXValue v t

     XVar _ n
      -> prettySeaName $ mangle n

     XApp{}
      | Just (p, xs) <- takePrimApps xx
      -> seaOfPrimDocApps (seaOfXPrim p) (fmap seaOfExp xs)

     _
      -> seaError "seaOfExp" xx

seaOfXValue :: BaseValue -> ValType -> Doc
seaOfXValue v t
 = case v of
     VError  err   -> seaOfError err
     VUnit         -> "iunit"
     VBool   True  -> "itrue"
     VBool   False -> "ifalse"
     VTime   x     -> seaOfTime x
     VString x     -> seaOfString x

     VInt x
      | isNaN      (fromIntegral x :: Double)
      -> nan
      | isInfinite (fromIntegral x :: Double)
      -> inf
      | otherwise
      -> pretty x
     VDouble x
      | isNaN      x
      -> nan
      | isInfinite x
      -> inf
      | otherwise
      -> pretty x

     VArray vs
      | ArrayT t' <- t
      -> let len = length vs
             writes arr (v', i :: Int)
              = prim (PrimArray $ PrimArrayPutMutable t')
                     [arr, pretty i, seaOfXValue v' t']
             init
              = prim (PrimUnsafe $ PrimUnsafeArrayCreate t')
                     [pretty len]
        in  foldl writes init (vs `List.zip` [0..])

      | otherwise
      -> seaError "seaOfXValue: array of wrong type" (v,t)

     VBuf []
      -> seaOfPrimDocApps
                     (PDFun (prefixOfValType t <> "make") Nothing)
                     []
     VBuf _
      -> seaError "seaOfXValue: buffer elements should be converted to pushes by convertValues " (v,t)

     VMap _
      -> seaError "seaOfXValue: maps should be removed by flatten" v
     _
      -> seaError "seaOfXValue: this should be removed by melt" v
 where
  prim p args
   = seaOfPrimDocApps (seaOfXPrim p) args
  -- assume math.h has these
  nan
   = "NAN"
  inf
   = "INFINITY"

seaOfError :: ExceptionInfo -> Doc
seaOfError e
 = case e of
     ExceptNotAnError
      -> "ierror_not_an_error"

     ExceptTombstone
      -> "ierror_tombstone"

     ExceptFold1NoValue
      -> "ierror_fold1_no_value"

     ExceptCannotCompute
      -> "ierror_cannot_compute"

     ExceptNotANumber
      -> "ierror_not_a_number"

     ExceptIndexOutOfBounds
      -> "ierror_index_out_of_bounds"

