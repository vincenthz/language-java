module Language.Java.Pretty where

import Text.PrettyPrint
import Text.Printf (printf)
import Data.Char (toLower)
import Data.List (intersperse)

import Language.Java.Syntax


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
  pretty :: a -> Doc
  pretty = prettyPrec 0
  
  prettyPrec :: Int -> a -> Doc
  prettyPrec _ = pretty

-----------------------------------------------------------------------
-- Packages

instance Pretty CompilationUnit where
  prettyPrec p (CompilationUnit mpd ids tds) =
    vcat $ ((maybePP p mpd): map (prettyPrec p) ids) ++ map (prettyPrec p) tds

instance Pretty PackageDecl where
  prettyPrec p (PackageDecl name) = text "package" <+> prettyPrec p name <> semi

instance Pretty ImportDecl where
  prettyPrec p (ImportDecl st name wc) =
    text "import" <+> opt st (text "static")
                  <+> prettyPrec p name <> opt wc (text ".*")
                  <> semi

-----------------------------------------------------------------------
-- Declarations

instance Pretty TypeDecl where
  prettyPrec p (ClassTypeDecl     cd) = prettyPrec p cd
  prettyPrec p (InterfaceTypeDecl id) = prettyPrec p id

instance Pretty ClassDecl where
  prettyPrec p (EnumDecl mods ident impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "enum"
          , prettyPrec p ident 
          , ppImplements p impls
         ] $$ prettyPrec p body

  prettyPrec p (ClassDecl mods ident tParams mSuper impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text "class"
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p (maybe [] return mSuper)
          , ppImplements p impls
         ] $$ prettyPrec p body

instance Pretty ClassBody where
  prettyPrec p (ClassBody ds) =
    braceBlock (map (prettyPrec p) ds)
    
instance Pretty EnumBody where
  prettyPrec p (EnumBody cs ds) =
    braceBlock $ 
        punctuate comma (map (prettyPrec p) cs) ++ 
        opt (not $ null ds) semi : map (prettyPrec p) ds

instance Pretty EnumConstant where
  prettyPrec p (EnumConstant ident args mBody) =
    prettyPrec p ident 
        -- needs special treatment since even the parens are optional
        <> opt (not $ null args) (ppArgs args) 
      $$ maybePP p mBody

instance Pretty InterfaceDecl where
  prettyPrec p (InterfaceDecl kind mods ident tParams impls body) =
    hsep [hsep (map (prettyPrec p) mods)
          , text (if kind == InterfaceNormal then "interface" else "@interface")
          , prettyPrec p ident
          , ppTypeParams p tParams
          , ppExtends p impls
         ] $$ prettyPrec p body

instance Pretty InterfaceBody where
  prettyPrec p (InterfaceBody mds) =
    braceBlock (map (prettyPrec p) mds)

instance Pretty Decl where
  prettyPrec p (MemberDecl md) = prettyPrec p md
  prettyPrec p (InitDecl b bl) =
    opt b (text "static") <+> prettyPrec p bl

instance Pretty MemberDecl where
  prettyPrec p (FieldDecl mods t vds) =
    hsep (map (prettyPrec p) mods ++ prettyPrec p t:punctuate (text ",") (map (prettyPrec p) vds)) <> semi

  prettyPrec p (MethodDecl mods tParams mt ident fParams throws def body) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , ppResultType p mt
          , prettyPrec p ident
          , ppArgs fParams
          , ppThrows p throws
          , ppDefault p def
         ] $$ prettyPrec p body

  prettyPrec p (ConstructorDecl mods tParams ident fParams throws body) =
    hsep [hsep (map (prettyPrec p) mods)
          , ppTypeParams p tParams
          , prettyPrec p ident
          , ppArgs fParams
          , ppThrows p throws
         ] $$ prettyPrec p body

  prettyPrec p (MemberClassDecl cd) = prettyPrec p cd
  prettyPrec p (MemberInterfaceDecl id) = prettyPrec p id

instance Pretty VarDecl where
  prettyPrec p (VarDecl vdId Nothing) = prettyPrec p vdId
  prettyPrec p (VarDecl vdId (Just ie)) =
    (prettyPrec p vdId <+> char '=') <+> prettyPrec p ie

instance Pretty VarDeclId where
  prettyPrec p (VarId ident) = prettyPrec p ident
  prettyPrec p (VarDeclArray vId) = prettyPrec p vId <> text "[]"

instance Pretty VarInit where
  prettyPrec p (InitExp e) = prettyPrec p e
  prettyPrec p (InitArray (ArrayInit ai)) =
    text "{" <+> hsep (punctuate comma (map (prettyPrec p) ai)) <+> text "}"

instance Pretty FormalParam where
  prettyPrec p (FormalParam mods t b vId) =
    hsep [hsep (map (prettyPrec p) mods)
          , prettyPrec p t <> opt b (text "...")
          , prettyPrec p vId
         ]

instance Pretty MethodBody where
  prettyPrec p (MethodBody mBlock) = maybe semi (prettyPrec p) mBlock

instance Pretty ConstructorBody where
  prettyPrec p (ConstructorBody mECI stmts) =
    braceBlock $ maybePP p mECI : map (prettyPrec p) stmts

instance Pretty ExplConstrInv where
  prettyPrec p (ThisInvoke rts args) =
    ppTypeParams p rts <+> text "this" <> ppArgs args <> semi
  prettyPrec p (SuperInvoke rts args) =
    ppTypeParams p rts <+> text "super" <> ppArgs args <> semi
  prettyPrec p (PrimarySuperInvoke e rts args) =
    prettyPrec p e <> char '.' <>
      ppTypeParams p rts <+> text "super" <> ppArgs args <> semi

instance Pretty Modifier where
  prettyPrec p (Annotation ann) = prettyPrec p ann $+$ nest (-1) ( text "")
  prettyPrec p mod = text . map toLower $ show mod

instance Pretty Annotation where
  prettyPrec p x = text "@" <> prettyPrec p (annName x) <> case x of
         MarkerAnnotation {} -> text ""
         SingleElementAnnotation {} -> text "(" <> prettyPrec p (annValue x) <> text ")"  
         NormalAnnotation {} -> text "(" <> ppEVList p (annKV x) <> text ")"

ppEVList p = hsep . punctuate comma . map (\(k,v) -> prettyPrec p k <+> text "=" <+> prettyPrec p v)

instance Pretty ElementValue where
  prettyPrec p (EVVal vi) = prettyPrec p vi
  prettyPrec p (EVAnn ann) = prettyPrec p ann

-----------------------------------------------------------------------
-- Statements


instance Pretty Block where
  prettyPrec p (Block stmts) = braceBlock $ map (prettyPrec p) stmts

instance Pretty BlockStmt where
  prettyPrec p (BlockStmt stmt) = prettyPrec p stmt
  prettyPrec p (LocalClass cd) = prettyPrec p cd
  prettyPrec p (LocalVars mods t vds) =
    hsep (map (prettyPrec p) mods) <+> prettyPrec p t <+> 
      hsep (punctuate comma $ map (prettyPrec p) vds) <> semi

instance Pretty Stmt where
  prettyPrec p (StmtBlock block) = prettyPrec p block
  prettyPrec p (IfThen c th) =
    text "if" <+> parens (prettyPrec 0 c) $+$ prettyNestedStmt 0 th

  prettyPrec p (IfThenElse c th el) =
    text "if" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 th $+$ text "else" $+$ prettyNestedStmt 0 el
      
  prettyPrec p (While c stmt) =
    text "while" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 stmt
  
  prettyPrec p (BasicFor mInit mE mUp stmt) =
    text "for" <+> (parens $ hsep [maybePP p mInit, semi
                           , maybePP p mE, semi
                           , maybe empty (hsep . punctuate comma . map (prettyPrec p)) mUp
                          ]) $+$ prettyNestedStmt p stmt

  prettyPrec p (EnhancedFor mods t ident e stmt) =
    hsep [text "for"
          , parens $ hsep [
                  hsep (map (prettyPrec p) mods)
                , prettyPrec p t
                , prettyPrec p ident
                , colon
                , prettyPrec p e
               ]
          , prettyPrec p stmt
         ]

  prettyPrec p Empty = semi
  
  prettyPrec p (ExpStmt e) = prettyPrec p e <> semi

  prettyPrec p (Assert ass mE) =
    text "assert" <+> prettyPrec p ass
      <+> maybe empty ((colon <>) . prettyPrec p) mE <> semi

  prettyPrec p (Switch e sBlocks) =
    text "switch" <+> parens (prettyPrec p e) 
      $$ braceBlock (map (prettyPrec p) sBlocks)

  prettyPrec p (Do stmt e) =
    text "do" $+$ prettyPrec p stmt <+> text "while" <+> parens (prettyPrec p e) <> semi
  
  prettyPrec p (Break mIdent) =
    text "break" <+> maybePP p mIdent <> semi
  
  prettyPrec p (Continue mIdent) =
    text "continue" <+> maybePP p mIdent <> semi
  
  prettyPrec p (Return mE) =
    text "return" <+> maybePP p mE <> semi
  
  prettyPrec p (Synchronized e block) =
    text "synchronized" <+> parens (prettyPrec p e) $$ prettyPrec p block
  
  prettyPrec p (Throw e) =
    text "throw" <+> prettyPrec p e <> semi
  
  prettyPrec p (Try block catches mFinally) =
    text "try" $$ prettyPrec p block $$
      vcat (map (prettyPrec p) catches ++ [ppFinally mFinally])
   where ppFinally Nothing = empty
         ppFinally (Just bl) = text "finally" <+> prettyPrec p bl

  prettyPrec p (Labeled ident stmt) =
    prettyPrec p ident <> colon <+> prettyPrec p stmt

instance Pretty Catch where
  prettyPrec p (Catch fParam block) =
    hsep [text "catch", parens (prettyPrec p fParam)] $$ prettyPrec p block

instance Pretty SwitchBlock where
  prettyPrec p (SwitchBlock lbl stmts) =
    vcat (prettyPrec p lbl : map (nest 2 . prettyPrec p) stmts)

instance Pretty SwitchLabel where
  prettyPrec p (SwitchCase e) =
    text "case" <+> prettyPrec p e <> colon
  prettyPrec p Default = text "default:"

instance Pretty ForInit where
  prettyPrec p (ForLocalVars mods t vds) =
    hsep $ map (prettyPrec p) mods ++
            prettyPrec p t: punctuate comma (map (prettyPrec p) vds)
  prettyPrec p (ForInitExps es) =
    hsep $ punctuate comma (map (prettyPrec p) es)


-----------------------------------------------------------------------
-- Expressions

instance Pretty Exp where
  prettyPrec p (Lit l) = prettyPrec p l
  
  prettyPrec p (ClassLit mT) =
    ppResultType p mT <> text ".class"

  prettyPrec _ This = text "this"
  
  prettyPrec p (ThisClass name) =
    prettyPrec p name <> text ".this"
    
  prettyPrec p (InstanceCreation tArgs tds args mBody) =
    hsep [text "new" 
          , ppTypeParams p tArgs 
          , prettyPrec p tds <> ppArgs args
         ] $$ maybePP p mBody
  
  prettyPrec p (QualInstanceCreation e tArgs ident args mBody) =
    hsep [prettyPrec p e <> char '.' <> text "new"
          , ppTypeParams p tArgs
          , prettyPrec p ident <> ppArgs args
         ] $$ maybePP p mBody

  prettyPrec p (ArrayCreate t es k) =
    text "new" <+> 
      hcat (prettyPrec p t : map (brackets . prettyPrec p) es 
                ++ replicate k (text "[]"))
  
  prettyPrec p (ArrayCreateInit t k init) =
    text "new" 
      <+> hcat (prettyPrec p t : replicate k (text "[]")) 
      <+> prettyPrec p init

  prettyPrec p (FieldAccess fa) = parenPrec p 1 $ prettyPrec 1 fa
  
  prettyPrec p (MethodInv mi) = parenPrec p 1 $ prettyPrec 1 mi
  
  prettyPrec p (ArrayAccess ain) = parenPrec p 1 $ prettyPrec 1 ain

  prettyPrec p (ExpName name) = prettyPrec p name
  
  prettyPrec p (PostIncrement e) = parenPrec p 1 $ prettyPrec 2 e <> text "++"

  prettyPrec p (PostDecrement e) = parenPrec p 1 $ prettyPrec 2 e <> text "--"

  prettyPrec p (PreIncrement e)  = parenPrec p 1 $ text "++" <> prettyPrec 2 e
  
  prettyPrec p (PreDecrement e)  = parenPrec p 1 $ text "--" <> prettyPrec 2 e

  prettyPrec p (PrePlus e) = parenPrec p 2 $ char '+' <> prettyPrec 2 e
  
  prettyPrec p (PreMinus e) = parenPrec p 2 $ char '-' <> prettyPrec 2 e
  
  prettyPrec p (PreBitCompl e) = parenPrec p 2 $ char '~' <> prettyPrec 2 e 

  prettyPrec p (PreNot e) = parenPrec p 2 $ char '!' <> prettyPrec 2 e

  prettyPrec p (Cast t e) = parenPrec p 2 $ parens (prettyPrec p t) <+> prettyPrec 2 e
  
  prettyPrec p (BinOp e1 op e2) =
    let prec = opPrec op in
    parenPrec p prec (prettyPrec prec e1 <+> prettyPrec p op <+> prettyPrec prec e2)

  prettyPrec p (InstanceOf e rt) =
    let cp = opPrec LThan in
    parenPrec p cp $ prettyPrec cp e
                   <+> text "instanceof" <+> prettyPrec cp rt
    
  prettyPrec p (Cond c th el) =
    parenPrec p 13 $ prettyPrec 13 c <+> char '?'
                   <+> prettyPrec p th <+> colon <+> prettyPrec 13 el

  prettyPrec p (Assign lhs aop e) =
    hsep [prettyPrec p lhs, prettyPrec p aop, prettyPrec p e]

  prettyPrec p (Lambda params body) =
    prettyPrec p params <+> text "->" <+> prettyPrec p body

  prettyPrec p (MethodRef i1 i2) =
    prettyPrec p i1 <+> text "::" <+> prettyPrec p i2

instance Pretty LambdaParams where
  prettyPrec p (LambdaSingleParam ident) = prettyPrec p ident
  prettyPrec p (LambdaFormalParams params) = ppArgs params
  prettyPrec p (LambdaInferredParams idents) = ppArgs idents

instance Pretty LambdaExpression where
  prettyPrec p (LambdaExpression exp) = prettyPrec p exp
  prettyPrec p (LambdaBlock block) = prettyPrec p block

instance Pretty Literal where
  prettyPrec p (Int i) = text (show i)
  prettyPrec p (Word i) = text (show i) <> char 'L'
  prettyPrec p (Float f) = text (show f) <> char 'F'
  prettyPrec p (Double d) = text (show d)
  prettyPrec p (Boolean b) = text . map toLower $ show b
  prettyPrec p (Char c) = quotes $ text (escapeChar c)
  prettyPrec p (String s) = doubleQuotes $ text (concatMap escapeString s)
  prettyPrec p (Null) = text "null"

instance Pretty Op where
  prettyPrec p op = text $ case op of
    Mult    -> "*"
    Div     -> "/"
    Rem     -> "%"
    Add     -> "+"
    Sub     -> "-"
    LShift  -> "<<"
    RShift  -> ">>"
    RRShift -> ">>>"
    LThan   -> "<"
    GThan   -> ">"
    LThanE  -> "<="
    GThanE  -> ">="
    Equal   -> "=="
    NotEq   -> "!="
    And     -> "&"
    Xor     -> "^"
    Or      -> "|"
    CAnd    -> "&&"
    COr     -> "||"
    
instance Pretty AssignOp where
  prettyPrec p aop = text $ case aop of
    EqualA  -> "="
    MultA   -> "*="
    DivA    -> "/="
    RemA    -> "%="
    AddA    -> "+="
    SubA    -> "-="
    LShiftA -> "<<="
    RShiftA -> ">>="
    RRShiftA -> ">>>="
    AndA    -> "&="
    XorA    -> "^="
    OrA     -> "|="

instance Pretty Lhs where
  prettyPrec p (NameLhs name) = prettyPrec p name
  prettyPrec p (FieldLhs fa) = prettyPrec p fa
  prettyPrec p (ArrayLhs ain) = prettyPrec p ain

instance Pretty ArrayIndex where
  prettyPrec p (ArrayIndex ref e) = prettyPrec p ref <> (hcat $ map (brackets . (prettyPrec p)) e)

instance Pretty FieldAccess where
  prettyPrec p (PrimaryFieldAccess e ident) =
    prettyPrec p e <> char '.' <> prettyPrec p ident
  prettyPrec p (SuperFieldAccess ident) =
    text "super." <> prettyPrec p ident
  prettyPrec p (ClassFieldAccess name ident) =
    prettyPrec p name <> text ".super." <> prettyPrec p ident

instance Pretty MethodInvocation where
  prettyPrec p (MethodCall name args) =
    prettyPrec p name <> ppArgs args

  prettyPrec p (PrimaryMethodCall e tArgs ident args) =
    hcat [prettyPrec p e, char '.', ppTypeParams p tArgs, 
           prettyPrec p ident, ppArgs args]

  prettyPrec p (SuperMethodCall tArgs ident args) =
    hcat [text "super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

  prettyPrec p (ClassMethodCall name tArgs ident args) =
    hcat [prettyPrec p name, text ".super.", ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]
  
  prettyPrec p (TypeMethodCall name tArgs ident args) =
    hcat [prettyPrec p name, char '.', ppTypeParams p tArgs,
           prettyPrec p ident, ppArgs args]

instance Pretty ArrayInit where
  prettyPrec p (ArrayInit vInits) =
    braceBlock $ map (\v -> prettyPrec p v <> comma) vInits
    --braces $ hsep (punctuate comma (map (prettyPrec p) vInits))


ppArgs :: Pretty a => [a] -> Doc
ppArgs = parens . hsep . punctuate comma . map pretty

-----------------------------------------------------------------------
-- Types

instance Pretty Type where
  prettyPrec p (PrimType pt) = prettyPrec p pt
  prettyPrec p (RefType  rt) = prettyPrec p rt

instance Pretty RefType where
  prettyPrec p (ClassRefType ct) = prettyPrec p ct
  prettyPrec p (ArrayType t) = prettyPrec p t <> text "[]"

instance Pretty ClassType where
  prettyPrec p (ClassType itas) =
    hcat . punctuate (char '.') $
      map (\(i,tas) -> prettyPrec p i <> ppTypeParams p tas) itas

instance Pretty TypeArgument where
  prettyPrec p (ActualType rt) = prettyPrec p rt
  prettyPrec p (Wildcard mBound) = char '?' <+> maybePP p mBound

instance Pretty TypeDeclSpecifier where
  prettyPrec p (TypeDeclSpecifier ct) = prettyPrec p ct
  prettyPrec p (TypeDeclSpecifierWithDiamond ct i d) =  prettyPrec p ct <> char '.' <> prettyPrec p i <> prettyPrec p d
  prettyPrec p (TypeDeclSpecifierUnqualifiedWithDiamond i d) = prettyPrec p i <> prettyPrec p d

instance Pretty Diamond where
  prettyPrec p Diamond = text "<>"

instance Pretty WildcardBound where
  prettyPrec p (ExtendsBound rt) = text "extends" <+> prettyPrec p rt
  prettyPrec p (SuperBound   rt) = text "super"   <+> prettyPrec p rt

instance Pretty PrimType where
  prettyPrec p BooleanT = text "boolean"
  prettyPrec p ByteT    = text "byte"
  prettyPrec p ShortT   = text "short"
  prettyPrec p IntT     = text "int"
  prettyPrec p LongT    = text "long"
  prettyPrec p CharT    = text "char"
  prettyPrec p FloatT   = text "float"
  prettyPrec p DoubleT  = text "double"

instance Pretty TypeParam where
  prettyPrec p (TypeParam ident rts) =
    prettyPrec p ident 
      <+> opt (not $ null rts) 
           (hsep $ text "extends": 
                    punctuate (text " &") (map (prettyPrec p) rts))

ppTypeParams :: Pretty a => Int -> [a] -> Doc
ppTypeParams _ [] = empty
ppTypeParams p tps = char '<' 
    <> hsep (punctuate comma (map (prettyPrec p) tps))
    <> char '>'

ppImplements :: Int -> [RefType] -> Doc
ppImplements _ [] = empty
ppImplements p rts = text "implements" 
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppExtends :: Int -> [RefType] -> Doc
ppExtends _ [] = empty
ppExtends p rts = text "extends" 
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppThrows :: Int -> [ExceptionType] -> Doc
ppThrows _ [] = empty
ppThrows p ets = text "throws" 
    <+> hsep (punctuate comma (map (prettyPrec p) ets))

ppDefault :: Int -> Maybe Exp -> Doc
ppDefault _ Nothing = empty
ppDefault p (Just exp) = text "default" <+> prettyPrec p exp

ppResultType :: Int -> Maybe Type -> Doc
ppResultType _ Nothing = text "void"
ppResultType p (Just a) = prettyPrec p a

-----------------------------------------------------------------------
-- Names and identifiers

instance Pretty Name where
  prettyPrec p (Name is) =
    hcat (punctuate (char '.') $ map (prettyPrec p) is)

instance Pretty Ident where
  prettyPrec p (Ident s) = text s


-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: Int -> Stmt -> Doc
prettyNestedStmt prio p@(StmtBlock b) = prettyPrec prio p
prettyNestedStmt prio p = nest 2 (prettyPrec prio p)

maybePP :: Pretty a => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

braceBlock :: [Doc] -> Doc
braceBlock xs = char '{'
    $+$ nest 2 (vcat xs)
    $+$ char '}'

opPrec Mult    = 3
opPrec Div     = 3
opPrec Rem     = 3
opPrec Add     = 4
opPrec Sub     = 4
opPrec LShift  = 5
opPrec RShift  = 5
opPrec RRShift = 5
opPrec LThan   = 6
opPrec GThan   = 6
opPrec LThanE  = 6
opPrec GThanE  = 6
opPrec Equal   = 7
opPrec NotEq   = 7
opPrec And     = 8
opPrec Xor     = 9
opPrec Or      = 10
opPrec CAnd    = 11
opPrec COr     = 12

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c | c >= ' ' && c < '\DEL' = [c]
                | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
                | otherwise = error $ "Language.Java.Pretty.escapeGeneral: Char " ++ show c ++ " too large for Java char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c | c <= '\xFFFF' = escapeGeneral c
               | otherwise = escapeGeneral lead ++ escapeGeneral trail
                   where c' = fromEnum c - 0x010000
                         lead = toEnum $ 0xD800 + c' `div` 0x0400
                         trail = toEnum $ 0xDC00 + c' `mod` 0x0400
