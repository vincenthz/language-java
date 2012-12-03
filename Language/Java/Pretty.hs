module Language.Java.Pretty where

import Text.PrettyPrint
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
  pretty (CompilationUnit mpd ids tds) =
    vcat $ maybePP mpd: map pretty ids ++ map pretty tds

instance Pretty PackageDecl where
  pretty (PackageDecl name) = text "package" <+> pretty name <> semi

instance Pretty ImportDecl where
  pretty (ImportDecl st name wc) =
    text "import" <+> opt st (text "static")
                  <+> pretty name <> opt wc (text ".*")
                  <> semi

-----------------------------------------------------------------------
-- Declarations

instance Pretty TypeDecl where
  pretty (ClassTypeDecl     cd) = pretty cd
  pretty (InterfaceTypeDecl id) = pretty id

instance Pretty ClassDecl where
  pretty (EnumDecl mods ident impls body) =
    hsep [hsep (map pretty mods)
          , text "enum"
          , pretty ident 
          , ppImplements impls
         ] $$ pretty body

  pretty (ClassDecl mods ident tParams mSuper impls body) =
    hsep [hsep (map pretty mods)
          , text "class"
          , pretty ident
          , ppTypeParams tParams
          , ppExtends (maybe [] return mSuper)
          , ppImplements impls
         ] $$ pretty body

instance Pretty ClassBody where
  pretty (ClassBody ds) =
    braceBlock (map pretty ds)
    
instance Pretty EnumBody where
  pretty (EnumBody cs ds) =
    braceBlock $ 
        punctuate comma (map pretty cs) ++ 
        opt (not $ null ds) semi : map pretty ds

instance Pretty EnumConstant where
  pretty (EnumConstant ident args mBody) =
    pretty ident 
        -- needs special treatment since even the parens are optional
        <> opt (not $ null args) (ppArgs args) 
      $$ maybePP mBody

instance Pretty InterfaceDecl where
  pretty (InterfaceDecl mods ident tParams impls body) =
    hsep [hsep (map pretty mods)
          , text "interface"
          , pretty ident
          , ppTypeParams tParams
          , ppImplements impls
         ] $$ pretty body

instance Pretty InterfaceBody where
  pretty (InterfaceBody mds) =
    braceBlock (map pretty mds)

instance Pretty Decl where
  pretty (MemberDecl md) = pretty md
  pretty (InitDecl b bl) =
    opt b (text "static") <+> pretty bl

instance Pretty MemberDecl where
  pretty (FieldDecl mods t vds) =
    hsep (map pretty mods ++ pretty t:map pretty vds) <> semi

  pretty (MethodDecl mods tParams mt ident fParams throws body) =
    hsep [hsep (map pretty mods)
          , ppTypeParams tParams
          , ppResultType mt
          , pretty ident
          , ppArgs fParams
          , ppThrows throws
         ] $$ pretty body

  pretty (ConstructorDecl mods tParams ident fParams throws body) =
    hsep [hsep (map pretty mods)
          , ppTypeParams tParams
          , pretty ident
          , ppArgs fParams
          , ppThrows throws
         ] $$ pretty body

  pretty (MemberClassDecl cd) = pretty cd
  pretty (MemberInterfaceDecl id) = pretty id

instance Pretty VarDecl where
  pretty (VarDecl vdId Nothing) = pretty vdId
  pretty (VarDecl vdId (Just ie)) =
	(pretty vdId <+> char '=') <+> pretty ie

instance Pretty VarDeclId where
  pretty (VarId ident) = pretty ident
  pretty (VarDeclArray vId) = pretty vId

instance Pretty VarInit where
  pretty (InitExp e) = pretty e
  pretty (InitArray (ArrayInit ai)) =
	text "{" <+> hsep (punctuate comma (map pretty ai)) <+> text "}"

instance Pretty FormalParam where
  pretty (FormalParam mods t b vId) =
    hsep [hsep (map pretty mods)
          , pretty t <> opt b (text "...")
          , pretty vId
         ]

instance Pretty MethodBody where
  pretty (MethodBody mBlock) = maybe semi pretty mBlock

instance Pretty ConstructorBody where
  pretty (ConstructorBody mECI stmts) =
    braceBlock $ maybePP mECI : map pretty stmts

instance Pretty ExplConstrInv where
  pretty (ThisInvoke rts args) =
    ppTypeParams rts <+> text "this" <> ppArgs args <> semi
  pretty (SuperInvoke rts args) =
    ppTypeParams rts <+> text "super" <> ppArgs args <> semi
  pretty (PrimarySuperInvoke e rts args) =
    pretty e <> char '.' <>
      ppTypeParams rts <+> text "super" <> ppArgs args <> semi

instance Pretty Modifier where
  pretty (Annotation ann) = pretty ann $+$ nest (-1) ( text "")
  pretty mod = text . map toLower $ show mod

instance Pretty Annotation where
  pretty x = text "@" <> pretty (annName x) <> case x of
         MarkerAnnotation {} -> text ""
         SingleElementAnnotation {} -> text "(" <> pretty (annValue x) <> text ")"  
         NormalAnnotation {} -> text "(" <> ppEVList (annKV x) <> text ")"

ppEVList = hsep . punctuate comma . map (\(k,v) -> pretty k <+> text "=" <+> pretty v)

instance Pretty ElementValue where
  pretty (EVVal vi) = pretty vi
  pretty (EVAnn ann) = pretty ann

-----------------------------------------------------------------------
-- Statements


instance Pretty Block where
  pretty (Block stmts) = braceBlock $ map pretty stmts

instance Pretty BlockStmt where
  pretty (BlockStmt stmt) = pretty stmt
  pretty (LocalClass cd) = pretty cd
  pretty (LocalVars mods t vds) =
    hsep (map pretty mods) <+> pretty t <+> 
      hsep (punctuate comma $ map pretty vds) <> semi

instance Pretty Stmt where
  pretty (StmtBlock block) = pretty block
  pretty (IfThen c th) =
    text "if" <+> parens (prettyPrec 0 c) $+$ prettyNestedStmt th

  pretty (IfThenElse c th el) =
    text "if" <+> parens (pretty c) $+$ prettyNestedStmt th $+$ text "else" $+$ prettyNestedStmt el
      
  pretty (While c stmt) =
    text "while" <+> parens (pretty c) $+$ prettyNestedStmt stmt
  
  pretty (BasicFor mInit mE mUp stmt) =
    text "for" <+> (parens $ hsep [maybePP mInit, semi
                           , maybePP mE, semi
                           , maybe empty (hsep . punctuate comma . map pretty) mUp
                          ]) $+$ prettyNestedStmt stmt

  pretty (EnhancedFor mods t ident e stmt) =
    hsep [text "for"
          , parens $ hsep [
                  hsep (map pretty mods)
                , pretty t
                , pretty ident
                , colon
                , pretty e
               ]
          , pretty stmt
         ]

  pretty Empty = semi
  
  pretty (ExpStmt e) = pretty e <> semi

  pretty (Assert ass mE) =
    text "assert" <+> pretty ass
      <+> maybe empty ((colon <>) . pretty) mE <> semi

  pretty (Switch e sBlocks) =
    text "switch" <+> parens (pretty e) 
      $$ braceBlock (map pretty sBlocks)

  pretty (Do stmt e) =
    text "do" $+$ pretty stmt <+> text "while" <+> parens (pretty e) <> semi
  
  pretty (Break mIdent) =
    text "break" <+> maybePP mIdent <> semi
  
  pretty (Continue mIdent) =
    text "continue" <+> maybePP mIdent <> semi
  
  pretty (Return mE) =
    text "return" <+> maybePP mE <> semi
  
  pretty (Synchronized e block) =
    text "synchronized" <+> parens (pretty e) $$ pretty block
  
  pretty (Throw e) =
    text "throw" <+> pretty e <> semi
  
  pretty (Try block catches mFinally) =
    text "try" $$ pretty block $$
      vcat (map pretty catches ++ [ppFinally mFinally])
   where ppFinally Nothing = empty
         ppFinally (Just bl) = text "finally" <+> pretty bl

  pretty (Labeled ident stmt) =
    pretty ident <> colon <+> pretty stmt

instance Pretty Catch where
  pretty (Catch fParam block) =
    hsep [text "catch", parens (pretty fParam)] $$ pretty block

instance Pretty SwitchBlock where
  pretty (SwitchBlock lbl stmts) =
    vcat (pretty lbl : map (nest 2 . pretty) stmts)

instance Pretty SwitchLabel where
  pretty (SwitchCase e) =
    text "case" <+> pretty e <> colon
  pretty Default = text "default:"

instance Pretty ForInit where
  pretty (ForLocalVars mods t vds) =
    hsep $ map pretty mods ++
            pretty t: punctuate comma (map pretty vds)
  pretty (ForInitExps es) =
    hsep $ punctuate comma (map pretty es)


-----------------------------------------------------------------------
-- Expressions

instance Pretty Exp where
  prettyPrec _ (Lit l) = pretty l
  
  prettyPrec _ (ClassLit mT) =
    ppResultType mT <> text ".class"

  prettyPrec _ This = text "this"
  
  prettyPrec _ (ThisClass name) =
    pretty name <> text ".this"
    
  prettyPrec _ (InstanceCreation tArgs ct args mBody) =
    hsep [text "new" 
          , ppTypeParams tArgs 
          , pretty ct <> ppArgs args
         ] $$ maybePP mBody
  
  prettyPrec _ (QualInstanceCreation e tArgs ident args mBody) =
    hsep [pretty e <> char '.' <> text "new"
          , ppTypeParams tArgs
          , pretty ident <> ppArgs args
         ] $$ maybePP mBody

  prettyPrec _ (ArrayCreate t es k) =
    text "new" <+> 
      hcat (pretty t : map (brackets . pretty) es 
                ++ replicate k (text "[]"))
  
  prettyPrec _ (ArrayCreateInit t k init) =
    text "new" 
      <+> hcat (pretty t : replicate k (text "[]")) 
      <+> pretty init

  prettyPrec p (FieldAccess fa) = parenPrec p 1 $ prettyPrec 1 fa
  
  prettyPrec p (MethodInv mi) = parenPrec p 1 $ prettyPrec 1 mi
  
  prettyPrec p (ArrayAccess ain) = parenPrec p 1 $ prettyPrec 1 ain

  prettyPrec _ (ExpName name) = pretty name
  
  prettyPrec p (PostIncrement e) = parenPrec p 2 $ prettyPrec 2 e <> text "++"

  prettyPrec p (PostDecrement e) = parenPrec p 2 $ prettyPrec 2 e <> text "--"

  prettyPrec p (PreIncrement e)  = parenPrec p 2 $ text "++" <> prettyPrec 2 e
  
  prettyPrec p (PreDecrement e)  = parenPrec p 2 $ text "--" <> prettyPrec 2 e

  prettyPrec p (PrePlus e) = parenPrec p 2 $ char '+' <> prettyPrec 2 e
  
  prettyPrec p (PreMinus e) = parenPrec p 2 $ char '-' <> prettyPrec 2 e
  
  prettyPrec p (PreBitCompl e) = parenPrec p 2 $ char '~' <> prettyPrec 2 e 

  prettyPrec p (PreNot e) = parenPrec p 2 $ char '!' <> prettyPrec 2 e

  prettyPrec p (Cast t e) = parenPrec p 2 $ parens (pretty t) <+> prettyPrec 2 e
  
  prettyPrec p (BinOp e1 op e2) =
    let prec = opPrec op in
    parenPrec p prec (prettyPrec prec e1 <+> pretty op <+> prettyPrec prec e2)

  prettyPrec p (InstanceOf e rt) =
    let cp = opPrec LThan in
    parenPrec p cp $ prettyPrec cp e
                   <+> text "instanceof" <+> prettyPrec cp rt
    
  prettyPrec p (Cond c th el) =
    parenPrec p 13 $ prettyPrec 13 c <+> char '?'
                   <+> pretty th <+> colon <+> prettyPrec 13 el

  prettyPrec _ (Assign lhs aop e) =
    hsep [pretty lhs, pretty aop, pretty e]


instance Pretty Literal where
  pretty (Int i) = text (show i)
  pretty (Word i) = text (show i) <> char 'L'
  pretty (Float f) = text (show f) <> char 'F'
  pretty (Double d) = text (show d)
  pretty (Boolean b) = text . map toLower $ show b
  pretty (Char c) = text (show c)
  pretty (String s) = text (show s)
  pretty (Null) = text "null"

instance Pretty Op where
  pretty op = text $ case op of
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
  pretty aop = text $ case aop of
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
  pretty (NameLhs name) = pretty name
  pretty (FieldLhs fa) = pretty fa
  pretty (ArrayLhs ain) = pretty ain

instance Pretty ArrayIndex where
  pretty (ArrayIndex ref e) = pretty ref <> brackets (pretty e)

instance Pretty FieldAccess where
  pretty (PrimaryFieldAccess e ident) =
    pretty e <> char '.' <> pretty ident
  pretty (SuperFieldAccess ident) =
    text "super." <> pretty ident
  pretty (ClassFieldAccess name ident) =
    pretty name <> text ".super." <> pretty ident

instance Pretty MethodInvocation where
  pretty (MethodCall name args) =
    pretty name <> ppArgs args

  pretty (PrimaryMethodCall e tArgs ident args) =
    hcat [pretty e, char '.', ppTypeParams tArgs, 
           pretty ident, ppArgs args]

  pretty (SuperMethodCall tArgs ident args) =
    hcat [text "super.", ppTypeParams tArgs,
           pretty ident, ppArgs args]

  pretty (ClassMethodCall name tArgs ident args) =
    hcat [pretty name, text ".super.", ppTypeParams tArgs,
           pretty ident, ppArgs args]
  
  pretty (TypeMethodCall name tArgs ident args) =
    hcat [pretty name, char '.', ppTypeParams tArgs,
           pretty ident, ppArgs args]

instance Pretty ArrayInit where
  pretty (ArrayInit vInits) =
    braceBlock $ map (\v -> pretty v <> comma) vInits
    --braces $ hsep (punctuate comma (map pretty vInits))


ppArgs :: Pretty a => [a] -> Doc
ppArgs = parens . hsep . punctuate comma . map pretty

-----------------------------------------------------------------------
-- Types

instance Pretty Type where
  pretty (PrimType pt) = pretty pt
  pretty (RefType  rt) = pretty rt

instance Pretty RefType where
  pretty (ClassRefType ct) = pretty ct
  pretty (ArrayType t) = pretty t <> text "[]"

instance Pretty ClassType where
  pretty (ClassType itas) =
    hcat . punctuate (char '.') $
      map (\(i,tas) -> pretty i <> ppTypeParams tas) itas

instance Pretty TypeArgument where
  pretty (ActualType rt) = pretty rt
  pretty (Wildcard mBound) = char '?' <+> maybePP mBound

instance Pretty WildcardBound where
  pretty (ExtendsBound rt) = text "extends" <+> pretty rt
  pretty (SuperBound   rt) = text "super"   <+> pretty rt

instance Pretty PrimType where
  pretty BooleanT = text "boolean"
  pretty ByteT    = text "byte"
  pretty ShortT   = text "short"
  pretty IntT     = text "int"
  pretty LongT    = text "long"
  pretty CharT    = text "char"
  pretty FloatT   = text "float"
  pretty DoubleT  = text "double"

instance Pretty TypeParam where
  pretty (TypeParam ident rts) =
    pretty ident 
      <+> opt (not $ null rts) 
           (hsep $ text "extends": 
                    punctuate (text " &") (map pretty rts))

ppTypeParams :: Pretty a => [a] -> Doc
ppTypeParams [] = empty
ppTypeParams tps = char '<' 
    <> hsep (punctuate comma (map pretty tps))
    <> char '>'

ppImplements :: [RefType] -> Doc
ppImplements [] = empty
ppImplements rts = text "implements" 
    <+> hsep (punctuate comma (map pretty rts))

ppExtends :: [RefType] -> Doc
ppExtends [] = empty
ppExtends rts = text "extends" 
    <+> hsep (punctuate comma (map pretty rts))

ppThrows :: [ExceptionType] -> Doc
ppThrows [] = empty
ppThrows ets = text "throws" 
    <+> hsep (punctuate comma (map pretty ets))

ppResultType :: Maybe Type -> Doc
ppResultType Nothing = text "void"
ppResultType (Just a) = pretty a

-----------------------------------------------------------------------
-- Names and identifiers

instance Pretty Name where
  pretty (Name is) =
    hcat (punctuate (char '.') $ map pretty is)

instance Pretty Ident where
  pretty (Ident s) = text s


-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: Stmt -> Doc
prettyNestedStmt p@(StmtBlock b) = pretty p
prettyNestedStmt p = nest 2 (pretty p)

maybePP :: Pretty a => Maybe a -> Doc
maybePP = maybe empty pretty

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
