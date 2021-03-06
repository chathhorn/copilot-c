--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

-- | Generates the code around the SBV functions to hold the state-updates,
-- external variables, etc.  Note: this just creates calls to SBV-generated
-- functions, it does not create them!  (Use the names from Common.hs to ensure
-- agreement on names.)

module Copilot.Compile.C.Driver
  ( driver
  , driverName
  ) where

import Prelude hiding (id)
import qualified Data.Map as M
import Data.List (intersperse)
import qualified System.IO as I
import Text.PrettyPrint.HughesPJ
import System.Directory

import Copilot.Compile.C.MetaTable
import Copilot.Compile.C.Queue (QueueSize)
import Copilot.Compile.C.Common
import Copilot.Compile.C.Params

import qualified Copilot.Core as C
import qualified Copilot.Core.Type.Show as C (showWithType, ShowType(..))
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

driverName :: Params -> String
driverName params = withPrefix (prefix params) "driver" ++ ".c"

--------------------------------------------------------------------------------

-- | Define a C function.
mkFunc :: String -> Doc -> Doc
mkFunc fnName doc =
     text "void" <+> text fnName
       <> parens (text "void") <+> lbrace $+$ nest 2 doc $+$ rbrace

mkArgs :: [Doc] -> Doc
mkArgs args = hsep (punctuate comma args)

-- | Call a C function.
mkFuncCall :: String -> [Doc] -> Doc
mkFuncCall f args = text f <> parens (mkArgs args)

--------------------------------------------------------------------------------

driver :: Params -> MetaTable -> C.Spec -> String -> String -> IO ()
driver params meta (C.Spec streams observers _ _) dir fileName = do
  let filePath = dir ++ '/' : driverName params
  let wr doc = I.appendFile filePath ((mkStyle doc) ++ "\n")
  wr $ text "a"
  removeFile filePath

  wr $    text "/*"
      <+> text "Driver for SBV program generated from Copilot."
      <+> text "*/"
  wr $ text "/*" <+> text "Edit as you see fit" <+> text "*/"
  wr $ text ""

  wr $ text "#include" <+> doubleQuotes (text fileName <> text ".h")
  wr $ text "#include" <+> doubleQuotes (text $ c99HeaderName $ prefix params)
  wr $ text ""

  wr $ text "/* Observers */"
  wr $ declObservers (prefix params) observers
  wr $ text ""

  wr $ text "/* Variables */"

  wr $ varDecls meta
  wr $ text ""

  wr copilot

  wr $ text "/* Idents */\n"
  wr $ text "/*@\n assigns \\nothing;\n */\nSBool ident_bool(SBool a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSWord8 ident_word8(SWord8 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSWord16 ident_word16(SWord16 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSWord32 ident_word32(SWord32 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSWord64 ident_word64(SWord64 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSInt8 ident_int8(SInt8 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSInt16 ident_int16(SInt16 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSInt32 ident_int32(SInt32 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSInt64 ident_int64(SInt64 a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSFloat ident_float(SFloat a) {return a;}"
  wr $ text "/*@\n assigns \\nothing;\n */\nSDouble ident_double(SDouble a) {return a;}"

  wr $ text ""
  wr driverFn
  wr $ text ""
  wr testFn

  where
  mkStyle :: Doc -> String
  mkStyle = renderStyle (style {lineLength = 80})

  driverFn :: Doc
  driverFn =
    mkFunc (withPrefix (prefix params) "step")
           (   mkFuncCall sampleExtsF    [] <> semi
            $$ mkFuncCall triggersF      [] <> semi
            $$ mkFuncCall observersF     [] <> semi
            $$ mkFuncCall updateStatesF  [] <> semi
            $$ mkFuncCall updateBuffersF [] <> semi
            $$ mkFuncCall updatePtrsF    [] <> semi
           )
  testFn :: Doc
  testFn = mkFunc (withPrefix (prefix params) "testing") $ text "for(;;) step();"

  copilot = vcat $ intersperse (text "")
    [ sampleExts meta
    , fireTriggers meta
    , updateObservers params meta
    , updateStates streams
    , updateBuffers meta
    , updatePtrs meta
    ]

--------------------------------------------------------------------------------

-- Declare global variables.

data Decl = Decl { retT    :: Doc
                 , declVar :: Doc
                 , initVal :: Doc }

varDecls :: MetaTable -> Doc
varDecls meta = vcat $ map varDecl (getVars meta)

  where
  getVars :: MetaTable -> [Decl]
  getVars MetaTable { streamInfoMap    = streams
                    , externVarInfoMap = externs
                    , externArrInfoMap = externArrs
                    , externFunInfoMap = externFuns }
    =
       map getTmpStVars strLst
    ++ map getQueueVars strLst
    ++ map getQueuePtrVars (map fst strLst)
    ++ map getExtVars (M.toList externs)
    ++ map getExtArrs (M.toList externArrs)
    ++ map getExtFuns (M.toList externFuns)
    where
    strLst = M.toList streams

  getTmpStVars :: (C.Id, C.Stream) -> Decl
  getTmpStVars (id, C.Stream { C.streamExprType  = t
                               , C.streamBuffer = que }) =
    Decl (retType t) (text $ mkTmpStVar id) getFirst
    where
    -- ASSUME queue is nonempty!
    getFirst = text (cShow $ C.showWithType C.Haskell t (headErr que))
    headErr [] = C.impossible "headErr" "copilot-sbv"
    headErr xs = head xs

  getQueueVars :: (C.Id, C.Stream) -> Decl
  getQueueVars (id, C.Stream { C.streamExprType = t
                             , C.streamBuffer = que }) =
    Decl (retType t)
         (text (mkQueueVar id) <> brackets (int $ length que))
         getInits
    where
    getInits = lbrace <+> vals <+> rbrace
      where
      vals = hcat $ punctuate (comma <> text " ")
                              (map (text . cShow . C.showWithType C.Haskell t)
                                   que)

  getQueuePtrVars :: C.Id -> Decl
  getQueuePtrVars id =
    Decl (retType queSize) (text $ mkQueuePtrVar id) (int 0)
    where
    queSize :: C.Type QueueSize
    queSize = C.typeOf

  getExtVars :: (C.Name, C.ExtVar) -> Decl
  getExtVars (var, C.ExtVar _ (C.UType { C.uTypeType = t })) =
    Decl (retType t) (text $ mkExtTmpVar var) (int 0)

  getExtArrs :: (Int, C.ExtArray) -> Decl
  getExtArrs (_, C.ExtArray { C.externArrayName     = name
                            , C.externArrayElemType = t
                            , C.externArrayTag      = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  getExtFuns :: (Int, C.ExtFun) -> Decl
  getExtFuns (_, C.ExtFun { C.externFunName = name
                          , C.externFunType = t
                          , C.externFunTag  = tag  })
    =
    Decl (retType t) (text $ mkExtTmpTag name tag) (int 0)

  varDecl :: Decl -> Doc
  varDecl Decl { retT = t, declVar = v, initVal = i } =
    text "static"<+> t <+> v <+> equals <+> i <> semi

  cShow :: String -> String
  cShow "True"  = show (1::Int)
  cShow "False" = show (0::Int)
  cShow x       = x

--------------------------------------------------------------------------------

declObservers :: Maybe String -> [C.Observer] -> Doc
declObservers prfx = vcat . map declObserver

  where
  declObserver :: C.Observer -> Doc
  declObserver
    C.Observer
      { C.observerName     = name
      , C.observerExprType = t } =
    retType t <+> text (withPrefix prfx name) <> semi

--------------------------------------------------------------------------------

sampleExts :: MetaTable -> Doc
sampleExts MetaTable { externVarInfoMap = extVMap
                     , externArrInfoMap = extAMap
                     , externFunInfoMap = extFMap }
  =
  -- Arrays and functions have to come after vars.  This is because we may use
  -- the assignment of extVars in the definition of extArrs.  The Analyzer.hs
  -- copilot-core prevents arrays or functions from being used in arrays or
  -- functions.
  mkFunc ("static " ++ sampleExtsF) $ vcat (extVars ++ extArrs ++ extFuns)

  where
  extVars = map sampleVExt ((fst . unzip . M.toList) extVMap)
  extArrs = map sampleAExt (M.toList extAMap)
  extFuns = map sampleFExt (M.toList extFMap)

--------------------------------------------------------------------------------

-- Variables

sampleVExt :: C.Name -> Doc
sampleVExt name =
  text (mkExtTmpVar name) <+> equals <+> text name <> semi

--------------------------------------------------------------------------------

sampleAExt :: (Int, C.ExtArray) -> Doc
sampleAExt (_, C.ExtArray { C.externArrayName = name
                          , C.externArrayIdx = idx
                          , C.externArrayTag = t     })
  = text (mkExtTmpTag name t) <+> equals <+> arrIdx name idx <> semi
  where
  arrIdx :: C.Name -> C.Expr a -> Doc
  arrIdx name' e = text name' <> brackets (idxFCall e) <> semi

  -- Ok, because the analyzer disallows arrays or function calls in index
  -- expressions, and we assign all variables before arrays.
  idxFCall :: C.Expr a -> Doc
  idxFCall e =
    mkFuncCall (mkExtArrFn name) (map text $ collectArgs e)

--------------------------------------------------------------------------------

sampleFExt :: (Int, C.ExtFun) -> Doc
sampleFExt (_, C.ExtFun { C.externFunName = name
                        , C.externFunArgs = args
                        , C.externFunTag  = tag  })
  =
  text (mkExtTmpTag name tag) <+> equals <+> text name
  <> parens (hsep $ punctuate comma $ map mkArgCall $ zip [(0 :: Int) ..] args)
  <> semi
  where
     mkArgCall :: (Int, C.UExpr) -> Doc
     mkArgCall (i, C.UExpr { C.uExprExpr = e }) =
       mkFuncCall (mkExtFunArgFn i name tag) (map text $ collectArgs e)

--------------------------------------------------------------------------------

updateStates :: [C.Stream] -> Doc
updateStates [] = (text "/*@\n assigns \\nothing;\n */") $$ (mkFunc ("static " ++ updateStatesF) $ vcat $ map updateSt [])
  where
  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text $ collectArgs e)
      <>  semi

updateStates streams = (text "/*@\n") <> (hcat $ map updateStACSL (streams)) <+> (text "*/") $$
  (mkFunc ("static " ++ updateStatesF) $ vcat $ map updateSt streams)
  where
  updateStACSL :: C.Stream -> Doc
  updateStACSL C.Stream { C.streamId  = id } =
    text " assigns "<> text (mkTmpStVar id) <> semi <> text "\n"


  updateSt :: C.Stream -> Doc
  updateSt C.Stream { C.streamId   = id
                    , C.streamExpr = e } =
    text (mkTmpStVar id) <+> equals
      <+> mkFuncCall (mkUpdateStFn id)
                     (map text $ collectArgs e)
      <>  semi

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> Doc
updateObservers params MetaTable { observerInfoMap = observers }
  = let ll = M.toList observers
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map updateObsvACSL (ll)) <+> (text "*/")
  ) $$
  (mkFunc ("static " ++ observersF) $ vcat $ map updateObsv (ll))
  where
  updateObsvACSL :: (C.Name, ObserverInfo) -> Doc
  updateObsvACSL (name, _) =
    text " assigns" <+> text (withPrefix (prefix params) name) <> semi <> text "\n"

  updateObsv :: (C.Name, ObserverInfo) -> Doc
  updateObsv (name, ObserverInfo { observerArgs = args }) =
    text (withPrefix (prefix params) name) <+> text "=" <+>
    mkFuncCall (mkObserverFn name) (map text args) <> semi

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Doc
fireTriggers MetaTable { triggerInfoMap = triggers }
  = mkFunc ("static " ++ triggersF) $ vcat $ map fireTrig (M.toList triggers)

  where
  -- if (guard) trigger(args);
  fireTrig :: (C.Name, TriggerInfo) -> Doc
  fireTrig (name, TriggerInfo { guardArgs      = gArgs
                              , triggerArgArgs = argArgs })
    =
    text "if" <+> parens guardF $+$ nest 2 f

    where
    f = text name <> parens (vcat $ punctuate comma $ map mkArg $ mkArgIdx argArgs) <> semi

    guardF :: Doc
    guardF = mkFuncCall (mkTriggerGuardFn name) (map text gArgs)

    mkArg :: (Int, [String]) -> Doc
    mkArg (i, args) = mkFuncCall (mkTriggerArgFn i name) (map text args)

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Doc
updateBuffers MetaTable { streamInfoMap = strMap }
  = let ll = M.toList strMap
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map updateBufACSL (ll)) <+> (text "*/")
  )$$
  (mkFunc ("static " ++ updateBuffersF) $ vcat $ map updateBuf (ll))

  where

  updateBufACSL :: (C.Id, C.Stream) -> Doc
  updateBufACSL (id, _) =
    updateFuncACSL (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFuncACSL :: String -> String -> String -> Doc
  updateFuncACSL que ptr tmp =
    text " assigns" <+> text que <> brackets (text ptr) <> semi <>
    (text "\n ensures" <+> text que <> brackets (text ptr) <+>  text "==" <+> text tmp <> semi <> text "\n")

  updateBuf :: (C.Id, C.Stream) -> Doc
  updateBuf (id, _) =
    updateFunc (mkQueueVar id) (mkQueuePtrVar id) (mkTmpStVar id)

  -- queue_strX[ptr] = newVal;
  updateFunc :: String -> String -> String -> Doc
  updateFunc que ptr tmp =
    text que <> brackets (text ptr) <+> equals <+> text tmp <> semi

--------------------------------------------------------------------------------

updatePtrs :: MetaTable -> Doc
updatePtrs MetaTable { streamInfoMap = strMap } =
  let ll = M.toList strMap
  in (case ll of
  [] -> text "/*@\n assigns \\nothing;\n */"
  _ -> (text "/*@\n") <> (hcat $ map varAndUpdateACSL (ll)) <+> (text "*/")
  )$$
  (mkFunc ("static " ++ updatePtrsF) $ vcat $ map varAndUpdate (ll))

  where

  varAndUpdateACSL :: (C.Id, C.Stream) -> Doc
  varAndUpdateACSL (id, C.Stream { C.streamBuffer = que }) =
    updateFuncACSL (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFuncACSL :: QueueSize -> String -> Doc
  updateFuncACSL sz ptr =
    text " assigns" <+> text ptr <> semi <> (text "\n ensures" <+> text ptr <+> text "=="
      <+> parens (text "\\old (" <> text ptr <+> text ") +" <+> int 1)
      <+> text "%" <+> int (fromIntegral sz) <> semi <> text "\n")


  varAndUpdate :: (C.Id, C.Stream) -> Doc
  varAndUpdate (id, C.Stream { C.streamBuffer = que }) =
    updateFunc (fromIntegral $ length que) (mkQueuePtrVar id)

  -- idx = (idx + 1) % queueSize;
  updateFunc :: QueueSize -> String -> Doc
  updateFunc sz ptr =
    text ptr <+> equals
      <+> parens (text ptr <+> text "+" <+> int 1)
      <+> text "%" <+> int (fromIntegral sz) <> semi

--------------------------------------------------------------------------------

sampleExtsF, triggersF, observersF, updatePtrsF :: String
updateBuffersF, updateStatesF :: String
updatePtrsF    = "updatePtrs"
updateBuffersF = "updateBuffers"
updateStatesF  = "updateStates"
triggersF      = "fireTriggers"
observersF     = "updateObservers"
sampleExtsF    = "sampleExts"

--------------------------------------------------------------------------------

retType :: C.Type a -> Doc
retType t = text $
  case t of
    C.Bool   -> "SBool"

    C.Int8   -> "SInt8"
    C.Int16  -> "SInt16"
    C.Int32  -> "SInt32"
    C.Int64  -> "SInt64"

    C.Word8  -> "SWord8"
    C.Word16 -> "SWord16"
    C.Word32 -> "SWord32"
    C.Word64 -> "SWord64"

    C.Float  -> "SFloat"
    C.Double -> "SDouble"
