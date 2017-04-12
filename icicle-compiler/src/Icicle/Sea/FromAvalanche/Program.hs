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
 where
  seaOfCompute attribute compute program
   = vsep $
    [ ""
    , "#line 1 \"compute function" <+> seaOfStateInfo attribute <+> pretty (nameOfCompute compute) <> "\""
    , "void " <> pretty (nameOfCompute compute) <> "(" <> pretty (nameOfStateType attribute) <+> "*s)"
    , "{"
    , indent 4 . vsep
               . fmap defOfAccumulator
               . Map.toList
               $ accumsOfProgram program `Map.union`
                 readsOfProgram  program
    , ""
    , indent 4 $ assign (defOfVar' 1 "anemone_mempool_t" "mempool") "s->mempool;"
    , indent 4 $ assign (defOfVar  0 TimeT (pretty $ textOfName $ bindtime program))
                        ("s->" <> stateInputTime attribute) <> ";"
    , ""
    , indent 4 (seaOfStatement attribute (statements program))
    , "}"
    ]

defOfAccumulator :: Pretty n => (Name n, ValType) -> Doc
defOfAccumulator (n, vt)
 = defOfVar 0 vt (seaOfName n) <> semi

------------------------------------------------------------------------

seaOfStatement :: (Show a, Show n, Pretty n, Eq n)
               => SeaProgramAttribute -> Statement (Annot a) n Prim -> Doc
seaOfStatement state stmt
 = case stmt of
     Block []
      -> Pretty.empty

     Block (s:[])
      -> seaOfStatement state s

     Block (s:ss)
      -> seaOfStatement state s <> line
      <> seaOfStatement state (Block ss)

     Let n xx stmt'
      | xt <- valTypeOfExp xx
      -> assign (defOfVar 0 xt (seaOfName n)) (seaOfExp xx) <> semi <> suffix "let" <> line
      <> seaOfStatement state stmt'

     If ii tt (Block [])
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement state tt)
              , "}"
              , ""
              ]

     If ii tt ee
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement state tt)
              , "} else {"
              , indent 4 (seaOfStatement state ee)
              , "}"
              , ""
              ]

     While t n _ end stmt'
      -> vsep [ "while (" <> seaOfName n <+> seaOfWhileType t <+> seaOfExp end <> ") {"
              , indent 4 $ seaOfStatement state stmt'
              , "}"
              ]
     ForeachInts t n start end stmt'
      -> vsep [ "for (iint_t" <+> seaOfName n <+> "=" <+> seaOfExp start <> ";"
                              <+> seaOfName n <+> seaOfForeachCompare t  <+> seaOfExp end <> ";"
                              <+> seaOfName n <>  seaOfForeachStep t     <> ") {"
              , indent 4 $ seaOfStatement state stmt'
              , "}"
              ]

     ForeachFacts (FactBinds ntime nfid ns) _ FactLoopNew stmt'
      -> let inputStruct = stateInputVars state
             structAssign (n, t)
                      = assign (defOfVar' 1 ("const" <+> seaOfValType t)
                                            ("const" <+> pretty newPrefix <> seaOfName n))
                               (stNew n) <> semi
             loopAssign (bindn, t) (inputn,_)
                      = assign (defOfVar 0 t (seaOfName bindn))
                               (pretty newPrefix <> seaOfName inputn <> "[i]") <> semi
             factTime = case reverse inputStruct of
                         [] -> seaError "seaOfStatement: no facts" ()
                         ((n,_):_) -> pretty newPrefix <> seaOfName n <> "[i]"
         in vsep $
            [ ""
            , assign (defOfVar' 0 ("const" <+> seaOfValType IntT) "new_count") (stCount <> ";")
            ] <> fmap structAssign inputStruct <>
            [ ""
            , "for (iint_t i = 0; i < new_count; i++) {"
            , indent 4 $ assign (defOfVar 0 FactIdentifierT (seaOfName nfid))  "i"      <> semi
            , indent 4 $ assign (defOfVar 0 TimeT           (seaOfName ntime)) factTime <> semi
            , indent 4 $ vsep (List.zipWith loopAssign ns inputStruct) <> line <> seaOfStatement state stmt'
            , "}"
            , ""
            ]

     InitAccumulator acc stmt'
      | Accumulator n _ xx <- acc
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> suffix "init" <> line
      <> seaOfStatement state stmt'

     Read n_val n_acc _ stmt'
      -> assign (seaOfName n_val) (seaOfName n_acc) <> semi <> suffix "read" <> line
      <> seaOfStatement state stmt'

     Write n xx
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> suffix "write"

     LoadResumable n _
      -> vsep [ ""
              , "if (" <> stHas n <> ") {"
              , indent 4 $ assign (seaOfName n) (stRes n) <> semi <> suffix "load"
              , "}" ]

     SaveResumable n _
      -> assign (stHas n) "itrue"       <> semi <> suffix "save" <> line
      <> assign (stRes n) (seaOfName n) <> semi <> suffix "save" <> line

     Output n _ xts
      | ixAssign <- \ix xx -> assign ("s->" <> seaOfNameIx n ix) (seaOfExp xx) <> semi <> suffix "output"
      -> vsep (List.zipWith ixAssign [0..] (fmap fst xts))

     -- TODO Implement historical facts

     ForeachFacts _ _ FactLoopHistory _
      -> Pretty.empty

     KeepFactInHistory _
      -> Pretty.empty
  where
   stNew   n = "s->" <> stateInputNew (seaOfName n)
   stRes   n = "s->" <> stateInputRes (seaOfName n)
   stHas   n = "s->" <> stateInputHas (seaOfName n)
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
      -> seaOfName n

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

