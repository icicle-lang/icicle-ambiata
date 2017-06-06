{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Program (
    seaOfPrograms
  , seaOfXValue
  , nameOfAttribute
  , nameOfAttribute'
  , nameOfCompute
  , nameOfCompute'
  , nameOfStateType
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Data

import           Icicle.Internal.Pretty
import qualified Icicle.Internal.Pretty as Pretty

import           Icicle.Sea.Error
import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.FromAvalanche.Type

import           P hiding (head)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ( NonEmpty(..) )

import qualified Data.Map as Map


------------------------------------------------------------------------

seaOfPrograms :: (Show a, Show n, Pretty n, Eq n)
             => Int -> Attribute -> NonEmpty (Program (Annot a) n Prim) -> Either SeaError Doc
seaOfPrograms name attrib programs = do
  state <- stateOfPrograms name attrib programs
  let computes = NonEmpty.zipWith (seaOfCompute state) (stateComputes state) programs
  pure $ vsep ( seaOfState state : NonEmpty.toList computes )

seaOfCompute ::
     (Show a, Show n, Pretty n, Eq n)
  => SeaProgramAttribute
  -> SeaProgramCompute
  -> Program (Annot a) n Prim
  -> Doc
seaOfCompute attribute compute program =
  let
    acc_names =
      fmap (first mangleToSeaName) . Map.toList $
        accumsOfProgram program `Map.union` readsOfProgram  program
    time_name =
      mangleToSeaName . bindtime $ program
    max_heap_name =
      mangleToSeaName . maxMapSize  $ program
  in
    vsep
      [ ""
      , "#line 1 \"compute function" <+> seaOfStateInfo attribute <+> pretty (nameOfCompute compute) <> "\""
      , "void " <> pretty (nameOfCompute compute) <> "(" <> pretty (nameOfStateType attribute) <+> "*s)"
      , "{"
      , indent 4 . vsep . fmap (uncurry defOfAccumulator) $ acc_names
      , ""
      , indent 4 $ assign (defOfVar' 1 "anemone_mempool_t" "mempool") "s->mempool;"
      , indent 4 $ assign
          (defOfVar  0 TimeT . prettyText . takeSeaName $ time_name)
          ("s->" <> stateInputTime attribute) <> ";"
      , indent 4 $ assign
          (defOfVar  0 IntT . prettyText . takeSeaName $ max_heap_name)
          "s->max_map_size;"
      , ""
      , indent 4 (seaOfStatement attribute compute (statements program))
      , "}"
      ]

defOfAccumulator :: SeaName -> ValType -> Doc
defOfAccumulator n t
 = defOfVar 0 t (prettyText . takeSeaName $ n) <> semi

------------------------------------------------------------------------

seaOfStatement :: (Show a, Show n, Pretty n, Eq n)
               => SeaProgramAttribute -> SeaProgramCompute -> Statement (Annot a) n Prim -> Doc
seaOfStatement state compute stmt
 = case stmt of
     Block []
      -> Pretty.empty

     Block (s:[])
      -> seaOfStatement state compute s

     Block (s:ss)
      -> seaOfStatement state compute s <> line
      <> seaOfStatement state compute (Block ss)

     Let name xx stmt'
      | n <- mangleToSeaName name
      , xt <- valTypeOfExp xx
      -> assign (defOfVar 0 xt . prettyText . takeSeaName $ n) (seaOfExp xx) <> semi <> suffix "let" <> line
      <> seaOfStatement state compute stmt'

     If ii tt (Block [])
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement state compute tt)
              , "}"
              , ""
              ]

     If ii tt ee
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement state compute tt)
              , "} else {"
              , indent 4 (seaOfStatement state compute ee)
              , "}"
              , ""
              ]

     While t name _ end stmt'
      | n <- mangleToSeaName name
      -> vsep [ "while (" <> prettyText (takeSeaName n) <+> seaOfWhileType t <+> seaOfExp end <> ") {"
              , indent 4 $ seaOfStatement state compute stmt'
              , "}"
              ]
     ForeachInts t name start end stmt'
      | n <- mangleToSeaName name
      -> vsep [ "for (iint_t" <+> (prettyText . takeSeaName $ n) <+> "=" <+> seaOfExp start <> ";"
                              <+> (prettyText . takeSeaName $ n) <+> seaOfForeachCompare t  <+> seaOfExp end <> ";"
                              <+> (prettyText . takeSeaName $ n) <>  seaOfForeachStep t     <> ") {"
              , indent 4 $ seaOfStatement state compute stmt'
              , "}"
              ]

     ForeachFacts (FactBinds ntime nfid ns) _ FactLoopNew stmt'
      | inputStruct <- stateInputVars state
      , nfid' <- mangleToSeaName nfid
      , ntime' <- mangleToSeaName ntime
      , ns' <- fmap (first mangleToSeaName) ns
      -> let
           structAssign :: (SeaName, ValType) -> Doc
           structAssign (n, t)
             | ndoc <- prettyText . takeSeaName $ n =
                 assign
                   (defOfVar' 1 ("const" <+> seaOfValType t) ("const" <+> pretty newPrefix <> ndoc))
                   (stNew n) <> semi

           loopAssign :: (SeaName, ValType) -> (SeaName, a) -> Doc
           loopAssign (bindn, t) (inputn,_) =
             assign (defOfVar 0 t . prettyText . takeSeaName $ bindn)
                    (pretty newPrefix <> (prettyText . takeSeaName $ inputn) <> "[i]") <> semi


           factTime = case reverse inputStruct of
                       [] -> seaError "seaOfStatement: no facts" ()
                       ((n,_):_) -> pretty newPrefix <> (prettyText . takeSeaName $ n) <> "[i]"

         in vsep $
            [ ""
            , assign (defOfVar' 0 ("const" <+> seaOfValType IntT) "new_count") (stCount <> ";")
            ] <> fmap structAssign inputStruct <>
            [ ""
            , "for (iint_t i = 0; i < new_count; i++) {"
            , indent 4 $
                assign (defOfVar 0 FactIdentifierT . prettyText . takeSeaName $ nfid') "i" <> semi
            , indent 4 $
                assign (defOfVar 0 TimeT . prettyText . takeSeaName $ ntime') factTime <> semi
            , indent 4 $
                vsep (List.zipWith loopAssign ns' inputStruct) <> line <> seaOfStatement state compute stmt'
            , "}"
            , ""
            ]

     InitAccumulator acc stmt'
      | Accumulator name _ xx <- acc
      , n <- mangleToSeaName name
      -> assign (prettyText . takeSeaName $ n) (seaOfExp xx) <> semi <> suffix "init" <> line
      <> seaOfStatement state compute stmt'

     Read name_val name_acc _ stmt'
      | n_val <- mangleToSeaName name_val
      , n_acc <- mangleToSeaName name_acc
      -> assign (prettyText . takeSeaName $ n_val) (prettyText . takeSeaName $ n_acc) <> semi <> suffix "read" <> line
      <> seaOfStatement state compute stmt'

     Write name xx
      | n <- mangleToSeaName name
      -> assign (prettyText . takeSeaName $ n) (seaOfExp xx) <> semi <> suffix "write"

     LoadResumable name _
      | n <- mangleToSeaName name
      , nd <- prettyText . takeSeaName $ n
      -> vsep [ ""
              , "if (" <> stHas n <> ") {"
              , indent 4 $ assign nd (stRes n) <> semi <> suffix "load"
              , "}" ]

     SaveResumable name _
      | n <- mangleToSeaName name
      , nd <- prettyText . takeSeaName $ n
      -> assign (stHas n) "itrue"       <> semi <> suffix "save" <> line
      <> assign (stRes n) nd <> semi <> suffix "save" <> line

     Output n _ xts
      | ixAssign <- \ix xx -> assign ("s->" <> (prettyText . takeSeaName . mangleToSeaNameIx n $ ix)) (seaOfExp xx) <> semi <> suffix "output"
      -> vsep (List.zipWith ixAssign [0..] (fmap fst xts))

     -- TODO Implement historical facts

     ForeachFacts _ _ FactLoopHistory _
      -> Pretty.empty

     KeepFactInHistory _
      -> Pretty.empty
  where
   stNew   n = "s->" <> stateInputNew (prettyText . takeSeaName $ n)
   stRes   n = "s->" <> stateInputRes (nameOfResumable compute . prettyText . takeSeaName $ n)
   stHas   n = "s->" <> stateInputHas (nameOfResumable compute . prettyText . takeSeaName $ n)
   stCount   = "s->" <> stateNewCount


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
      -> prettyText . takeSeaName . mangleToSeaName $ n

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
      -> int x
     VDouble x
      | isNaN      x
      -> nan
      | isInfinite x
      -> inf
      | otherwise
      -> double x

     VArray vs
      | ArrayT t' <- t
      -> let len = length vs
             writes arr (v',i)
              = prim (PrimArray $ PrimArrayPutMutable t')
                     [arr, int i, seaOfXValue v' t']
             init
              = prim (PrimUnsafe $ PrimUnsafeArrayCreate t')
                     [int len]
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

     ExceptScalarVariableNotAvailable
      -> "ierror_variable_not_available"

