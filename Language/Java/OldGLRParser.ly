> {
> module Language.Java.Parser where
>
> import Language.Java.Lexer
> import Language.Java.Syntax
 import Language.Java.ParseUtils
>
> import Data.Maybe (catMaybes)
> }

> %tokentype { Token }
> %token
>   'abstract'      { KW_Abstract     }
>   'assert'        { KW_Assert       }
>   'boolean'       { KW_Boolean      }
>   'break'         { KW_Break        }
>   'byte'          { KW_Byte         }
>   'case'          { KW_Case         }
>   'catch'         { KW_Catch        }
>   'char'          { KW_Char         }
>   'class'         { KW_Class        }
>   'const'         { KW_Const        }
>   'continue'      { KW_Continue     }
>   'default'       { KW_Default      }
>   'do'            { KW_Do           }
>   'double'        { KW_Double       }
>   'else'          { KW_Else         }
>   'enum'          { KW_Enum         }
>   'extends'       { KW_Extends      }
>   'final'         { KW_Final        }
>   'finally'       { KW_Finally      }
>   'float'         { KW_Float        }
>   'for'           { KW_For          }
>   'goto'          { KW_Goto         }
>   'if'            { KW_If           }
>   'implements'    { KW_Implements   }
>   'import'        { KW_Import       }
>   'instanceof'    { KW_Instanceof   }
>   'int'           { KW_Int          }
>   'interface'     { KW_Interface    }
>   'long'          { KW_Long         }
>   'native'        { KW_Native       }
>   'new'           { KW_New          }
>   'package'       { KW_Package      }
>   'private'       { KW_Private      }
>   'protected'     { KW_Protected    }
>   'public'        { KW_Public       }
>   'return'        { KW_Return       }
>   'short'         { KW_Short        }
>   'static'        { KW_Static       }
>   'strictfp'      { KW_Strictfp     }
>   'super'         { KW_Super        }
>   'switch'        { KW_Switch       }
>   'synchronized'  { KW_Synchronized }
>   'this'          { KW_This         }
>   'throw'         { KW_Throw        }
>   'throws'        { KW_Throws       }
>   'transient'     { KW_Transient    }
>   'try'           { KW_Try          }
>   'void'          { KW_Void         }
>   'volatile'      { KW_Volatile     }
>   'while'         { KW_While        }

>   '('             { OpenParen     }
>   ')'             { CloseParen    }
>   '['             { OpenSquare    }
>   ']'             { CloseSquare   }
>   '{'             { OpenCurly     }
>   '}'             { CloseCurly    }
>   ';'             { SemiColon     }
>   ','             { Comma         }
>   '.'             { Period        }

>   INT             { IntTok $$ }
>   LONG            { LongTok $$ }
>   DOUBLE          { DoubleTok $$ }
>   FLOAT           { FloatTok $$ }
>   CHAR            { CharTok $$ }
>   STRING          { StringTok $$ }
>   BOOLEAN         { BoolTok $$ }
>   NULL            { NullTok }

>   IDENT           { IdentTok $$ }

>   '='             { Op_Equal     }
>   '>'             { Op_GThan     }
>   '<'             { Op_LThan     }
>   '!'             { Op_Bang      }
>   '~'             { Op_Tilde     }
>   '?'             { Op_Query     }
>   ':'             { Op_Colon     }
>   '=='            { Op_Equals    }
>   '<='            { Op_LThanE    }
>   '>='            { Op_GThanE    }
>   '!='            { Op_BangE     }
>   '&&'            { Op_AAnd      }
>   '||'            { Op_OOr       }
>   '++'            { Op_PPlus     }
>   '--'            { Op_MMinus    }
>   '+'             { Op_Plus      }
>   '-'             { Op_Minus     }
>   '*'             { Op_Star      }
>   '/'             { Op_Slash     }
>   '&'             { Op_And       }
>   '|'             { Op_Or        }
>   '^'             { Op_Caret     }
>   '%'             { Op_Percent   }
>   '<<'            { Op_LShift    }
>   '>>'            { Op_RShift    }
>   '>>>'           { Op_RRShift   }
>   '+='            { Op_PlusE     }
>   '-='            { Op_MinusE    }
>   '*='            { Op_StarE     }
>   '/='            { Op_SlashE    }
>   '&='            { Op_AndE      }
>   '|='            { Op_OrE       }
>   '^='            { Op_CaretE    }
>   '%='            { Op_PercentE  }
>   '<<='           { Op_LShiftE   }
>   '>>='           { Op_RShiftE   }
>   '>>>='          { Op_RRShiftE  }


> %name mparse compilationUnit
> %%

----------------------------------------------------------------------------
-- Packages and compilation units

> compilationUnit   :: { CompilationUnit }
>       : optPackageDecl listImportDecl listTypeDecl      
>               { CompilationUnit $1 $2 (catMaybes $3) }

> optPackageDecl    :: { Maybe PackageDecl }
>       : packageDecl       { Just $1 }
>       |                   { Nothing }

> packageDecl       :: { PackageDecl }
>       : 'package' name ';'            { PackageDecl $2 }

> listImportDecl    :: { [ImportDecl] }
>       : importDecl listImportDecl     { $1 : $2 }
>       |                               { [] }

> importDecl        :: { ImportDecl }
>       : 'import' boptStatic name boptDotStar ';'        
>               { ImportDecl $2 $3 $4 }

> boptStatic        :: { Bool }
>       : 'static'          { True }
>       |                   { False }

> boptDotStar       :: { Bool }
>       : dotStar       { True }
>       |               { False }

> dotStar :: { () }
>       : '.' '*'                       { () }

> listTypeDecl    :: { [TypeDecl] }
>       : typeDecl listTypeDecl     { $1 : $2 }
>       |                               { [] }

> typeDecl          :: { Maybe TypeDecl }
>       : classOrInterfaceDecl          { Just $1 }
>       | ';'                           { Nothing }

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

> classOrInterfaceDecl  :: { TypeDecl }
>       : listModifier classDecl                        { ClassTypeDecl ($2 $1) }
-- TODO: modifier = public private protected abstract static strictfp final
>       | listModifier interfaceDecl                    { InterfaceTypeDecl ($2 $1) }
-- TODO: modifier = public private protected abstract static strictfp

> classDecl         :: { Mod ClassDecl }
>       : normalClassDecl               { $1 }
>       | enumClassDecl                 { $1 }

> normalClassDecl   :: { Mod ClassDecl }
>       : 'class' ident loptTypeParams optExtends loptImplements classBody
>               { \ms -> ClassDecl ms $2 $3 ((fmap head) $4) $5 $6 }
-- TODO: check that the extends clause only contains one type.

> enumClassDecl     :: { Mod ClassDecl }
>       : 'enum' ident loptImplements enumBody
>               { \ms -> EnumDecl ms $2 $3 $4 }

> optExtends        :: { Maybe [RefType] }
>       : extends       { Just $1 }
>       |               { Nothing }

> loptExtends       :: { [RefType] }
>       : optExtends    { maybe [] id $1 }

> extends           :: { [RefType] }
>       : 'extends' refTypeList                 { $2 }

> loptImplements    :: { [RefType] }
>       : implements    { $1 }
>       |               { [] }

> implements        :: { [RefType] }
>       : 'implements' refTypeList              { $2 }

> optClassBody      :: { Maybe ClassBody }
>       : classBody                             { Just $1 }
>       |                                       { Nothing }

> classBody         :: { ClassBody }
>       : '{' classBodyDecls '}'                { ClassBody $2 }

> enumBody          :: { EnumBody }
>       : '{' seplistEnumConstComma optComma loptEnumBodyDecls '}'
>               { EnumBody $2 $4 }

> optComma          :: { () }
>       : ','       { () }
>       |           { () }

> loptEnumBodyDecls     :: { [Decl] }
>       : enumBodyDecls     { $1 }
>       |                   { [] }

> enumBodyDecls     :: { [Decl] }
>       : ';' classBodyDecls                    { $2 }

> seplistEnumConstComma :: { [EnumConstant] }
>       : seplist1EnumConstComma                { $1 }
>       |                                       { [] }

> seplist1EnumConstComma :: { [EnumConstant] }
>       : enumConst ',' seplist1EnumConstComma  { $1 : $3 }
>       | enumConst                             { [$1] }

> enumConst         :: { EnumConstant }
>       : ident loptArgs optClassBody
>               { EnumConstant $1 $2 $3 }

> classBodyDecls    :: { [Decl] }
>       : listClassBodyDecl                     { $1 }

-- Interface declarations

> interfaceDecl     :: { Mod InterfaceDecl }
>       : 'interface' ident loptTypeParams loptExtends interfaceBody
>               { \ms -> InterfaceDecl ms $2 $3 $4 $5 }

> interfaceBody     :: { InterfaceBody }
>       : '{' listInterfaceBodyDecl '}'             { InterfaceBody (catMaybes $2) }

-- Declarations

> listClassBodyDecl     :: { [Decl] }
>       : classBodyDecl listClassBodyDecl   { $1 : $2 }
>       |                                   { [] }

> classBodyDecl         :: { Decl }
>       : listModifier memberDecl       { MemberDecl ($2 $1) }
-- TODO: Check that the modifiers on the memberDecl are valid
>       | boptStatic block              { InitDecl $1 $2 }

> memberDecl            :: { Mod MemberDecl }
>       : fieldDecl         { $1 }
>       | methodDecl        { $1 }
>       | constrDecl        { $1 }
>       | classDecl         { MemberClassDecl . $1 }
>       | interfaceDecl     { MemberInterfaceDecl . $1 }

> fieldDecl             :: { Mod MemberDecl }
>       : type varDecls     { \ms -> FieldDecl ms $1 $2 }

> methodDecl            :: { Mod MemberDecl }
>       : loptTypeParams resultType ident formalParams loptThrows methodBody
>               { \ms -> MethodDecl ms $1 $2 $3 $4 $5 $6 }

> methodBody            :: { MethodBody }
>       : ';'                               { MethodBody Nothing }
>       | block                             { MethodBody (Just $1) }

> constrDecl            :: { Mod MemberDecl }
>       : loptTypeParams ident formalParams  loptThrows constrBody
>               { \ms -> ConstructorDecl ms $1 $2 $3 $4 $5 }

> constrBody            :: { ConstructorBody }
>       : '{' optExplConstrInv listBlockStmt '}'
>               { ConstructorBody $2 $3 }

> optExplConstrInv      :: { Maybe ExplConstrInv }
>       : explConstrInv     { Just $1 }
>       |                   { Nothing }

> explConstrInv         :: { ExplConstrInv }
>       :             loptRefTypeArgs 'this'  args ';'      { ThisInvoke  $1 $3 }
>       |             loptRefTypeArgs 'super' args ';'      { SuperInvoke $1 $3 }
>       | primary '.' loptRefTypeArgs 'super' args ';'      { PrimarySuperInvoke $1 $3 $5 }

> listInterfaceBodyDecl     :: { [Maybe MemberDecl] }
>       : interfaceBodyDecl listInterfaceBodyDecl           { $1 : $2 }
>       |                                                   { [] }

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
> interfaceBodyDecl     :: { Maybe MemberDecl }
>       : listModifier interfaceMemberDecl              { Just ($2 $1) }
>       | ';'                                           { Nothing }

> interfaceMemberDecl   :: { Mod MemberDecl }
>       : fieldDecl         { $1 }
>       | absMethodDecl     { $1 }
>       | classDecl         { MemberClassDecl . $1 }
>       | interfaceDecl     { MemberInterfaceDecl . $1 }

> absMethodDecl         :: { Mod MemberDecl }
>       : loptTypeParams resultType ident formalParams loptThrows ';'
>               { \ms -> MethodDecl ms $1 $2 $3 $4 $5 (MethodBody Nothing) }

> loptThrows            :: { [RefType] }
>       : throws            { $1 }
>       |                   { [] }

> throws                :: { [RefType] }
>       : 'throws' seplist1RefTypeComma             { $2 }


-- Formal parameters

> formalParams             :: { [FormalParam] }
>       : '(' formalParamsAux ')'                           { $2 }

> formalParamsAux          :: { [FormalParam] }
>       : {- empty -}                                       { [] }
>       | lastFormalParam                                   { [$1] }
>       | seplist1FormalParamComma ',' lastFormalParam      { $1 ++ [$3] }

> lastFormalParam       :: { FormalParam }
>       : listModifier type optEllipsis varDeclId
>           { FormalParam $1 $2 $3 $4 }         -- TODO: modifier = final

> seplist1FormalParamComma  :: { [FormalParam] }
>       : formalParam ',' seplist1FormalParamComma          { $1 : $3 }
>       | formalParam                                       { [$1] }

> formalParam           :: { FormalParam }
>       : listModifier type varDeclId
>           { FormalParam $1 $2 False $3 }      -- TODO: modifier = final

> optEllipsis           :: { Bool }
>       : '.' '.' '.'       { True }
>       | {- empty -}       { False }


-- Modifiers

> listModifier  :: { [Modifier] }
>       : modifier listModifier     { $1 : $2 }
>       |                           { [] }

> modifier      :: { Modifier }
>       : 'public'                  { Public    }
>       | 'protected'               { Protected }
>       | 'private'                 { Private   }
>       | 'abstract'                { Abstract  }
>       | 'static'                  { Static    }
>       | 'strictfp'                { StrictFP  }
>       | 'final'                   { Final }
>       | 'native'                  { Native }
>       | 'transient'               { Transient }
>       | 'volatile'                { Volatile }

----------------------------------------------------------------------------
-- Variable declarations

> varDecls          :: { [VarDecl] }
>       : seplist1VarDeclComma              { $1 }

> seplist1VarDeclComma  :: { [VarDecl] }
>       : varDecl ',' seplist1VarDeclComma  { $1 : $3 }
>       | varDecl                           { [$1] }

> varDecl           :: { VarDecl }
>       : varDeclId optVarInit              { VarDecl $1 $2 }

> varDeclId         :: { VarDeclId }
>       : ident listArrBrackets
>               { foldr (\_ f -> VarDeclArray . f) VarId $2 $1 }

> listArrBrackets   :: { [()] }
>       : arrBrackets listArrBrackets       { $1 : $2 }
>       |                                   { [] }

> arrBrackets       :: { () }
>       : '[' ']'                           { () }

> localVarDecl      :: { ([Modifier], Type, [VarDecl]) }
>       : listModifier type varDecls      { ($1,$2,$3) }
        -- TODO: modifier = public protected private static final transient volatile

> optVarInit            :: { Maybe VarInit }
>       : varInit       { Just $1 }
>       |               { Nothing }

> seplistVarInitComma   :: { [VarInit] }
>       : seplist1VarInitComma              { $1 }
>       |                                   { [] }

> seplist1VarInitComma  :: { [VarInit] }
>       : varInit ',' seplist1VarInitComma  { $1 : $3 }
>       | varInit                           { [$1] }

> varInit               :: { VarInit }
>       : '=' exp                   { InitExp $2 }
>       | '=' arrayInit             { InitArray $2 }

> arrayInit             :: { ArrayInit }
>       : '{' seplistVarInitComma optComma '}' { ArrayInit $2 }

----------------------------------------------------------------------------
-- Statements

> block                 :: { Block }
>       : '{' listBlockStmt '}'                     { Block $2 }

> listBlockStmt         :: { [BlockStmt] }
>       : blockStmt listBlockStmt                   { $1 : $2 }
>       |                                           { [] }

> blockStmt             :: { BlockStmt }
>       : listModifier classDecl                    { LocalClass ($2 $1) }
>       | localVarDecl ';'    { let (m,t,vds) = $1 in LocalVars m t vds }
>       | stmt                                      { BlockStmt $1 }

> stmt                  :: { Stmt }
>       : ident ':' stmt                            { Labeled $1 $3 }
>       | 'if' '(' exp ')' stmt                     { IfThen $3 $5 }
>       | 'if' '(' exp ')' stmtNSI 'else' stmt      { IfThenElse $3 $5 $7 }
>       | 'while' '(' exp ')' stmt                  { While $3 $5 }
>       | 'for' '(' optForInit ';' optExp ';' optForUp ')' stmt
>               { BasicFor $3 $5 $7 $9 }
>       | 'for' '(' listModifier type ident ':' exp ')' stmt
>               { EnhancedFor $3 $4 $5 $7 $9 }
>       | stmtNoTrail                               { $1 }

> stmtNoTrail           :: { Stmt }
>       : block                                     { StmtBlock $1 }
>       | ';'                                       { Empty }
>       | stmtExp ';'                               { ExpStmt $1 }
>       | 'assert' exp optAssertExp2 ';'            { Assert $2 $3  }
>       | 'switch' '(' exp ')' switchBlock          { Switch $3 $5 }
>       | 'do' stmt 'while' '(' exp ')' ';'         { Do $2 $5 }
>       | 'break' optIdent ';'                      { Break $2 }
>       | 'continue' optIdent ';'                   { Continue $2 }
>       | 'return' optExp ';'                       { Return $2 }
>       | 'synchronized' '(' exp ')' block          { Synchronized $3 $5 }
>       | 'throw' exp ';'                           { Throw $2 }
>       | 'try' block catches                       { Try $2 $3 Nothing }
>       | 'try' block loptCatches 'finally' block   { Try $2 $3 (Just $5) }

> stmtNSI               :: { Stmt }
>       : stmtNoTrail                               { $1 }
>       | ident ':' stmtNSI                         { Labeled $1 $3 }
>       | 'if' '(' exp ')' stmtNSI 'else' stmtNSI   { IfThenElse $3 $5 $7 }
>       | 'while' '(' exp ')' stmtNSI               { While $3 $5 }
>       | 'for' '(' optForInit ';' optExp ';' optForUp ')' stmtNSI
>               { BasicFor $3 $5 $7 $9 }
>       | 'for' '(' listModifier type ident ':' exp ')' stmtNSI
>               { EnhancedFor $3 $4 $5 $7 $9 }

> optAssertExp2         :: { Maybe Exp }
>       : assertExp2        { Just $1 }
>       |                   { Nothing }

> assertExp2            :: { Exp }
>       : ':' exp                                   { $2 }

-- Switches

> switchBlock           :: { [SwitchBlock] }
>       : '{' listSwitchStmt '}'                    { $2 }

> listSwitchStmt        :: { [SwitchBlock] }
>       : switchStmt listSwitchStmt                 { $1 : $2 }
>       |                                           { [] }

> switchStmt            :: { SwitchBlock }
>       : switchLabel listBlockStmt                 { SwitchBlock $1 $2 }

> switchLabel           :: { SwitchLabel }
>       : 'case' exp ':'                            { SwitchCase $2 }
>       | 'default' ':'                             { Default }

-- For loops

> optForInit            :: { Maybe ForInit }
>       : forInit       { Just $1 }
>       |               { Nothing }

> forInit               :: { ForInit }
>       : localVarDecl            { let (m,t,vds) = $1 in ForLocalVars m t vds }
>       | seplist1StmtExpComma                          { ForInitExps $1 }

> optForUp              :: { Maybe [Exp] }
>       : forUp         { Just $1 }
>       |               { Nothing }

> forUp                 :: { [Exp] }
>       : seplist1StmtExpComma                          { $1 }

-- Try-catch clauses

> loptCatches           :: { [Catch] }
>       : catches       { $1 }
>       |               { [] }

> catches               :: { [Catch] }
>       : list1Catch                                     { $1 }

> list1Catch             :: { [Catch] }
>       : catch listCatch               { $1 : $2 }
>       | catch                         { [$1] }

> catch                 :: { Catch }
>       : 'catch' '(' formalParam ')' block         { Catch $3 $5 }

----------------------------------------------------------------------------
-- Expressions

> seplist1StmtExpComma      :: { [Exp] }
>       : stmtExp ',' seplist1StmtExpComma      { $1 : $3 }
>       | stmtExp                               { [$1] }

> stmtExp :: { Exp }
>       : postIncDec                { $1 }
>       | preIncDec                 { $1 }
>       | assignment                { $1 }
>       | methodInvocation          { MethodInv $1 }
>       | instanceCreation          { $1 }

> postIncDec            :: { Exp }
>       : postfixExp postfixOp      { $2 $1 }

> preIncDec             :: { Exp }
>       : preIncDecOp unaryExp      { $1 $2 }

> assignment            :: { Exp }
>       : lhs assignOp assignExp        { Assign $1 $2 $3 }

> lhs                   :: { Lhs }
>       : name                          { NameLhs $1 }
>       | fieldAccess                   { FieldLhs $1 }
>       | arrayAccess                   { ArrayLhs (fst $1) (snd $1) }

> optExp                :: { Maybe Exp }
>       : exp       { Just $1 }
>       |           { Nothing }

> seplistExpComma       :: { [Exp] }
>       : seplist1ExpComma      { $1 }
>       |                       { [] }

> seplist1ExpComma      :: { [Exp] }
>       : exp ',' seplist1ExpComma      { $1 : $3 }
>       | exp                           { [$1] }

> exp                   :: { Exp }
>       : assignExp                     { $1 }

> assignExp             :: { Exp }
>       : assignment                    { $1 }
>       | condExp                       { $1 }

> condExp               :: { Exp }
>       : condExp '?' exp ':' condExp   { Cond $1 $3 $5 }
>       | infixExp                      { $1 }

-- TODO: Fix precedence
> infixExp              :: { Exp }
>       : infixExp infixOp unaryExp     { BinOp $1 $2 $3 }
>       | infixExp 'instanceof' refType { InstanceOf $1 $3 }
>       | unaryExp                      { $1 }

> unaryExp              :: { Exp }
>       : preIncDec                     { $1 }
>       | prefixOp unaryExp             { $1 $2 }
>       | '(' type ')' unaryExp         { Cast $2 $4 }
>       | postfixExp                    { $1 }


> postfixExp            :: { Exp }
>       : primary               { $1 }
>       | name                  { ExpName $1 }
>       | postIncDec            { $1 }


> primary               :: { Exp }
>       : primaryNoNewArray         { $1 }
>       | arrayCreation             { $1 }

> primaryNoNewArray     :: { Exp }
>       : literal                   { Lit $1 }
>       | resultType '.' 'class'    { ClassLit $1 }
>       | 'this'                    { This }
>       | name '.' 'this'           { ThisClass $1 }
>       | '(' exp ')'               { Paren $2 }
>       | instanceCreation          { $1 }
>       | fieldAccess               { FieldAccess $1 }
>       | methodInvocation          { MethodInv $1 }
>       | arrayAccess               { ArrayAccess (fst $1) (snd $1) }

> instanceCreation      :: { Exp }
>       :             'new' loptTypeArgs classType args optClassBody
>              { InstanceCreation $2 $3 $4 $5 }
>       | primary '.' 'new' loptTypeArgs ident args optClassBody
>              { QualInstanceCreation $1 $4 $5 $6 $7 }

> fieldAccess          :: { FieldAccess }
>       : primary '.' ident             { PrimaryFieldAccess $1 $3 }
>       | 'super' '.' ident             { SuperFieldAccess      $3 }
>       | name '.' 'super' '.' ident    { ClassFieldAccess   $1 $5 }

> methodInvocation     :: { MethodInvocation }
>       : name args                 { MethodCall $1 $2 }
>       | primary '.' loptRefTypeArgs ident args      
>               { PrimaryMethodCall $1 $3 $4 $5 }
>       | 'super' '.' loptRefTypeArgs ident args
>               { SuperMethodCall      $3 $4 $5 }
>       | name '.' 'super' '.' loptRefTypeArgs ident args
>               { ClassMethodCall   $1 $5 $6 $7 }
>       | name '.' loptRefTypeArgs ident args
>               { TypeMethodCall    $1 $3 $4 $5 }

> loptArgs              :: { [Exp] }
>       : args      { $1 }
>       |           { [] }

> args                  :: { [Exp] }
>       : '(' seplistExpComma ')'               { $2 }

-- Arrays

> arrayAccess            :: { (Exp, Exp) }
>       : arrayRef '[' exp ']'          { ($1, $3) }

> arrayRef              :: { Exp }
>       : name                          { ExpName $1 }
>       | primaryNoNewArray             { $1 }

> arrayCreation         :: { Exp }
>       : 'new' type list1DimExpr dims          { ArrayCreate $2 $3 $4 }
>       | 'new' type dims1 arrayInit            { ArrayCreateInit $2 $3 $4 }

> list1DimExpr          :: { [Exp] }
>       : dimExpr list1DimExpr      { $1 : $2 }
>       | dimExpr                   { [$1] }

> dimExpr               :: { Exp }
>       : '[' exp ']'                   { $2 }

> dims                  :: { Int }
>       : optDims1                      { maybe 0 id $1 }

> optDims1              :: { Maybe Int }
>       : dims1     { Just $1 }
>       |           { Nothing }

> dims1                 :: { Int }
>       : list1Dim                      { length $1 }

> list1Dim              :: { [()] }
>       : dim list1Dim      { $1 : $2 }
>       | dim               { [$1] }

> dim                   :: { () }
>       : '[' ']'                       { () }

-- Literals

> literal               :: { Literal }
>       : INT                           { Int $1 }
>       | LONG                          { Word $1 }
>       | FLOAT                         { Float $1 }
>       | DOUBLE                        { Double $1 }
>       | BOOLEAN                       { Boolean $1 }
>       | CHAR                          { Char $1 }
>       | STRING                        { String $1 }
>       | NULL                          { Null }


-- Operators

> infixOp               :: { Op }
>       : '*'                       { Mult    }
>       | '/'                       { Div     }
>       | '%'                       { Rem     }
>       | '+'                       { Add     }
>       | '-'                       { Sub     }
>       | '<<'                      { LShift  }
>       | '>>'                      { RShift  }
>       | '>>>'                     { RRShift }
>       | '<'                       { LThan   }
>       | '>'                       { GThan   }
>       | '<='                      { LThanE  }
>       | '>='                      { GThanE  }
>       | '=='                      { Equal   }
>       | '!='                      { NotEq   }
>       | '&'                       { And     }
>       | '^'                       { Xor     }
>       | '|'                       { Or      }
>       | '&&'                      { CAnd    }
>       | '||'                      { COr     }

> assignOp              :: { AssignOp }
>       : '='                           { EqualA }
>       | '*='                          { MultA }
>       | '/='                          { DivA }
>       | '%='                          { RemA }
>       | '+='                          { AddA }
>       | '-='                          { SubA }
>       | '<<='                         { LShiftA }
>       | '>>='                         { RShiftA }
>       | '>>>='                        { RRShiftA }
>       | '&='                          { AndA }
>       | '^='                          { XorA }
>       | '|='                          { OrA }

> preIncDecOp           :: { Exp -> Exp }
>       : '++'                          { PreIncrement }
>       | '--'                          { PreDecrement }

> prefixOp              :: { Exp -> Exp }
>       : '!'                           { PreNot }
>       | '~'                           { PreBitCompl }
>       | '+'                           { PrePlus }
>       | '-'                           { PreMinus }

> postfixOp             :: { Exp -> Exp }
>       : '++'                          { PostIncrement }
>       | '--'                          { PostDecrement }

----------------------------------------------------------------------------
-- Types

> type          :: { Type }
>       : refType               { RefType $1 }
>       | primType              { PrimType $1 }

> primType      :: { PrimType }
>       : 'boolean'             { BooleanT  }
>       | 'byte'                { ByteT     }
>       | 'short'               { ShortT    }
>       | 'int'                 { IntT      }
>       | 'long'                { LongT     }
>       | 'char'                { CharT     }
>       | 'float'               { FloatT    }
>       | 'double'              { DoubleT   }

> seplist1RefTypeAnd    :: { [RefType] }
>       : refType '&' seplist1RefTypeAnd    { $1 : $3 }
>       | refType                           { [$1] }

> refType       :: { RefType }
>       : type '[' ']'                          { ArrayType $1 }
>       | classType                             { ClassRefType $1 }
-- No longer relevant:
       | ident                                 { TypeVariable $1 }

> classType     :: { ClassType }
>       : seplist1ClassTypeSpecPeriod           { ClassType $1 }

> seplist1ClassTypeSpecPeriod   :: { [(Ident, [TypeArgument])] }
>       : classTypeSpec '.' seplist1ClassTypeSpecPeriod     { $1 : $3 }
>       | classTypeSpec                                     { [$1] }

> classTypeSpec :: { (Ident, [TypeArgument]) }
>       : ident loptTypeArgs                    { ($1, $2) }

> resultType  :: { Maybe Type }
>       : 'void'                { Nothing }
>       | type                  { Just $1 }

> refTypeList    :: { [RefType] }
>       : seplist1RefTypeComma     { $1 }

> seplist1RefTypeComma  :: { [RefType] }
>       : refType ',' seplist1RefTypeComma      { $1 : $3 }
>       | refType                               { [1] }

----------------------------------------------------------------------------
-- Type parameters and arguments

> loptTypeParams    :: { [TypeParam] }
>       : typeParams        { $1 }
>       |                   { [] }

> typeParams    :: { [TypeParam] }
>       : '<' seplist1TypeParamComma '>'    { $2 }

> seplist1TypeParamComma        :: { [TypeParam] }
>       : typeParam ',' seplist1TypeParamComma      { $1 : $3 }
>       | typeParam                                 { [$1] }

> typeParam     :: { TypeParam }
>       : ident loptBounds                  { TypeParam $1 $2 }

> loptBounds    :: { [RefType] }
>       : bounds        { $1 }
>       |               { [] }

> bounds        :: { [RefType] }
>       : 'extends' seplist1RefTypeAnd      { $2 }

> loptTypeArgs  :: { [TypeArgument] }
>       : typeArgs      { $1 }
>       |               { [] }

> typeArgs      :: { [TypeArgument] }
>       : '<' seplist1TypeArgComma '>'      { $2 }

> seplist1TypeArgComma      :: { [TypeArgument] }
>       : typeArg ',' seplist1TypeArgComma  { $1 : $3 }
>       | typeArg                           { [$1] }

> typeArg       :: { TypeArgument }
>       : refType                           { ActualType $1 }
>       | '?' optWildcardBound              { Wildcard $2 }

> optWildcardBound  :: { Maybe WildcardBound }
>       : wildcardBound     { Just $1 }
>       |                   { Nothing }

> wildcardBound :: { WildcardBound }
>       : 'extends' refType                 { ExtendsBound $2 }
>       | 'super' refType                   { SuperBound $2 }

> loptRefTypeArgs   :: { [RefType] }
>       : refTypeArgs       { $1 }
>       |                   { [] }

> refTypeArgs   :: { [RefType] }
>       : '<' refTypeList '>'               { $2 }

----------------------------------------------------------------------------
-- Names

> name  :: { Name }
>       : seplist1IdentComma   { Name $1 }

> optIdent  :: { Maybe Ident }
>       : ident     { Just $1 }
>       |           { Nothing }

> seplist1IdentComma    :: { [Ident] }
>       : ident ',' seplist1IdentComma  { $1 : $3 }
>       | ident                         { [$1] }

> ident :: { Ident }
>       : IDENT                 { Ident $1 }

----------------------------------------------------------------------------
-- Higher-order productions

 opt(p)    : p                     { Just $1 }
           |                       { Nothing }
    
 lopt(p)   : opt(p)                { maybe [] id $1 }

 bopt(p)   : opt(p)                { maybe False (const True) $1 }
    
 fopt(p)   : opt(p)                { maybe id id $1 }

 list(p)   : list1(p)              { $1 }
           |                       { [] }
    
 list1(p)  : p                     { [$1] }
           | p list1(p)            { $1 : $2 }

 seplist(p,s)  : seplist1(p,s)     { $1 }
               |                   { [] }

 seplist1(p,s) : p                     { [$1] }
               | p s seplist1(p,s)     { $1 : $3 }

 flist(p)  : list(p)               { foldr (.) id $1 }


----------------------------------------------------------------------------
-- Helper functions

> {

> happyError = undefined

> type Mod a = [Modifier] -> a

> }
