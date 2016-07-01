--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.ACSLExpr
  ( ppExpr
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType, ShowType(..))
import Prelude hiding (id)
import Text.PrettyPrint.HughesPJ
import Copilot.Compile.C.Common
import Copilot.Compile.C.MetaTable as MT hiding (ExternFun)
import qualified Data.Map as M

--------------------------------------------------------------------------------

strmName :: Int -> Doc
strmName id = text "queue_" <> int id


ptrName :: Int -> Doc
ptrName id = text "ptr_" <> int id

--------------------------------------------------------------------------------

ppExpr :: MT.MetaTable -> Expr a -> Doc
ppExpr meta e0 = case e0 of
  Const t x                  -> text (showWithType Haskell t x)
  Drop _ 0 id                ->
        case M.lookup id (MT.streamInfoMap meta) of
          Just Stream { streamBuffer = ll } ->
            let streamSize = length ll in
            case streamSize of
              1 -> strmName id <> lbrack <> text "0" <> rbrack
              _ -> strmName id <> lbrack <> ptrName id <> rbrack
          Nothing -> empty
  Drop _ i id                ->
        case M.lookup id (MT.streamInfoMap meta) of
          Just Stream { streamBuffer = ll } ->
            let streamSize = length ll in
            strmName id <> lbrack <> lparen <> ptrName id <> text (" + " ++ show i) <> rparen <> text " % " <> int streamSize <> rbrack
          Nothing -> empty
  ExternVar _ name _        -> text $ mkExtTmpVar name
  ExternFun _ name _ _ tag  -> text $ mkExtTmpTag name tag
  ExternArray _ _ name
              _ _ _ tag      -> text $ mkExtTmpTag name tag
  Local _ _ name e1 e2       -> parens $ text "\\let" <+> text name <+> equals
                                          <+> ppExpr meta e1 <+> text ";" <+> ppExpr meta e2
  Var _ name                 -> text name
  Op1 op e                   -> parens $ ppOp1 op (ppExpr meta e)
  Op2 op e1 e2               -> parens $ ppOp2 op (ppExpr meta e1) (ppExpr meta e2)
  Op3 op e1 e2 e3            -> parens $ ppOp3 op (ppExpr meta e1) (ppExpr meta e2) (ppExpr meta e3)
  Label _ _ e                -> ppExpr meta e
  _                          -> empty

ppOp1 :: Op1 a b -> Doc -> Doc
ppOp1 op = case op of
  Not      -> ppPrefix "!"
  Abs _    -> ppPrefix "\\abs"
  Sign _   -> \x -> ((parens $ x <> (text " > 0")) <> text "? 1 : ") <> (parens $ x <> (text " < 0 ? -1 : ") <> x)
  Recip _  -> ppPrefix "\\recip"
  Exp _    -> ppPrefix "\\exp"
  Sqrt _   -> ppPrefix "\\sqrt"
  Log _    -> ppPrefix "\\log"
  Sin _    -> ppPrefix "\\sin"
  Tan _    -> ppPrefix "\\tan"
  Cos _    -> ppPrefix "\\cos"
  Asin _   -> ppPrefix "\\asin"
  Atan _   -> ppPrefix "\\atan"
  Acos _   -> ppPrefix "\\acos"
  Sinh _   -> ppPrefix "\\sinh"
  Tanh _   -> ppPrefix "\\tanh"
  Cosh _   -> ppPrefix "\\cosh"
  Asinh _  -> ppPrefix "\\asinh"
  Atanh _  -> ppPrefix "\\atanh"
  Acosh _  -> ppPrefix "\\acosh"
  BwNot _  -> ppPrefix "~"
  Cast _ _ -> ppPrefix ""

ppOp2 :: Op2 a b c -> Doc -> Doc -> Doc
ppOp2 op = case op of
  And          -> ppInfix "&&"
  Or           -> ppInfix "||"
  Add      _   -> ppInfix "+"
  Sub      _   -> ppInfix "-"
  Mul      _   -> ppInfix "*"
  Div      _   -> ppInfix "/"
  Mod      _   -> ppInfix "%"
  Fdiv     _   -> ppInfix "/"
  Pow      _   -> ppPrefix2 "\\pow"
  Logb     _   -> \ a b -> (text "(\\log" <> (a) <> (text ") / ( \\log") <> (b) <> text ")")
  Eq       _   -> ppInfix "=="
  Ne       _   -> ppInfix "!="
  Le       _   -> ppInfix "<="
  Ge       _   -> ppInfix ">="
  Lt       _   -> ppInfix "<"
  Gt       _   -> ppInfix ">"
  BwAnd    _   -> ppInfix "&"
  BwOr     _   -> ppInfix "|"
  BwXor    _   -> ppInfix "^"
  BwShiftL _ _ -> ppInfix "<<"
  BwShiftR _ _ -> ppInfix ">>"

ppOp3 :: Op3 a b c d -> Doc -> Doc -> Doc -> Doc
ppOp3 op = case op of
  Mux _    -> \ doc1 doc2 doc3 ->
    text "("   <+> doc1 <+>
    text "?" <+> doc2 <+>
    text ":" <+> doc3 <> text ")"

--------------------------------------------------------------------------------

ppInfix :: String -> Doc -> Doc -> Doc
ppInfix cs doc1 doc2 = parens $ doc1 <+> text cs <+> doc2

ppPrefix2 :: String -> Doc -> Doc -> Doc
ppPrefix2 cs doc1 doc2 = parens $ text cs <+> doc1 <> text "," <+> doc2

ppPrefix :: String -> Doc -> Doc
ppPrefix cs doc = (text cs <+> lparen <> doc <> rparen)

