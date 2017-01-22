{-# LANGUAGE CPP #-}
module Language.Java.Parser (
    parser,

    compilationUnit, packageDecl, importDecl, typeDecl,

    classDecl, interfaceDecl,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl,

    formalParams, formalParam,

    modifier,

    varDecls, varDecl,

    block, blockStmt, stmt,

    stmtExp, exp, primary, literal,

    ttype, primType, refType, classType, resultType,

    lambdaExp, methodRef,

    typeParams, typeParam,

    name, ident,


    empty, list, list1, seplist, seplist1, opt, bopt, lopt,

    comma, semiColon, period, colon

    ) where

import Language.Java.Lexer ( L(..), Token(..), lexer)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)

import Text.Parsec hiding ( Empty )
import Text.Parsec.Pos

import Prelude hiding ( exp, catch, (>>), (>>=) )
import qualified Prelude as P ( (>>), (>>=) )
import Data.Maybe ( isJust, catMaybes )
import Control.Monad ( ap )

#if __GLASGOW_HASKELL__ < 707
import Control.Applicative ( (<$>), (<$), (<*) )
-- Since I cba to find the instance Monad m => Applicative m declaration.
(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap
infixl 4 <*>
#else
import Control.Applicative ( (<$>), (<$), (<*), (<*>) )
#endif

type P = Parsec [L Token] ()

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=
-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

----------------------------------------------------------------------------
-- Top-level parsing

parseCompilationUnit :: String -> Either ParseError CompilationUnit
parseCompilationUnit inp =
    runParser compilationUnit () "" (lexer inp)

parser p = runParser p () "" . lexer

--class Parse a where
--  parse :: String -> a

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P CompilationUnit
compilationUnit = do
    mpd <- opt packageDecl
    ids <- list importDecl
    tds <- list typeDecl
    eof
    return $ CompilationUnit mpd ids (catMaybes tds)

packageDecl :: P PackageDecl
packageDecl = do
    tok KW_Package
    n <- name
    semiColon
    return $ PackageDecl n

importDecl :: P ImportDecl
importDecl = do
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- name
    ds <- bopt $ period >> tok Op_Star
    semiColon
    return $ ImportDecl st n ds

typeDecl :: P (Maybe TypeDecl)
typeDecl = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = do
    ms <- list modifier
    de <- (do cd <- classDecl
              return $ \ms -> ClassTypeDecl (cd ms)) <|>
          (do id <- annInterfaceDecl <|> interfaceDecl
              return $ \ms -> InterfaceTypeDecl (id ms))
    return $ de ms

classDecl :: P (Mod ClassDecl)
classDecl = normalClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod ClassDecl)
normalClassDecl = do
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extends
    imp <- lopt implements
    bod <- classBody
    return $ \ms -> ClassDecl ms i tps ((fmap head) mex) imp bod

extends :: P [RefType]
extends = tok KW_Extends >> refTypeList

implements :: P [RefType]
implements = tok KW_Implements >> refTypeList

enumClassDecl :: P (Mod ClassDecl)
enumClassDecl = do
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    return $ \ms -> EnumDecl ms i imp bod

classBody :: P ClassBody
classBody = ClassBody <$> braces classBodyStatements

enumBody :: P EnumBody
enumBody = braces $ do
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ EnumBody ecs eds

enumConst :: P EnumConstant
enumConst = do
    id  <- ident
    as  <- lopt args
    mcb <- opt classBody
    return $ EnumConstant id as mcb

enumBodyDecls :: P [Decl]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod InterfaceDecl)
annInterfaceDecl = do
    tok KW_AnnInterface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> InterfaceDecl InterfaceAnnotation ms id tps exs bod

interfaceDecl :: P (Mod InterfaceDecl)
interfaceDecl = do
    tok KW_Interface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> InterfaceDecl InterfaceNormal ms id tps exs bod

interfaceBody :: P InterfaceBody
interfaceBody = InterfaceBody . catMaybes <$>
    braces (list interfaceBodyDecl)

-- Declarations

classBodyStatement :: P (Maybe Decl)
classBodyStatement =
    (try $ do
       list1 semiColon
       return Nothing) <|>
    (try $ do
       mst <- bopt (tok KW_Static)
       blk <- block
       return $ Just $ InitDecl mst blk) <|>
    (do ms  <- list modifier
        dec <- memberDecl
        return $ Just $ MemberDecl (dec ms))

memberDecl :: P (Mod MemberDecl)
memberDecl =
    (try $ do
        cd  <- classDecl
        return $ \ms -> MemberClassDecl (cd ms)) <|>
    (try $ do
        id  <- try annInterfaceDecl <|> try interfaceDecl
        return $ \ms -> MemberInterfaceDecl (id ms)) <|>

    try fieldDecl <|>
    try methodDecl <|>
    constrDecl

fieldDecl :: P (Mod MemberDecl)
fieldDecl = endSemi $ do
    typ <- ttype
    vds <- varDecls
    return $ \ms -> FieldDecl ms typ vds

methodDecl :: P (Mod MemberDecl)
methodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBody
    return $ \ms -> MethodDecl ms tps rt id fps thr Nothing bod

methodBody :: P MethodBody
methodBody = MethodBody <$>
    (const Nothing <$> semiColon <|> Just <$> block)


constrDecl :: P (Mod MemberDecl)
constrDecl = do
    tps <- lopt typeParams
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBody
    return $ \ms -> ConstructorDecl ms tps id fps thr bod

constrBody :: P ConstructorBody
constrBody = braces $ do
    mec <- opt (try explConstrInv)
    bss <- list blockStmt
    return $ ConstructorBody mec bss

explConstrInv :: P ExplConstrInv
explConstrInv = endSemi $
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ ThisInvoke tas as) <|>
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ SuperInvoke tas as) <|>
    (do pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ PrimarySuperInvoke pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe MemberDecl)
interfaceBodyDecl = semiColon >> return Nothing <|>
    do ms  <- list modifier
       imd <- interfaceMemberDecl
       return $ Just (imd ms)

interfaceMemberDecl :: P (Mod MemberDecl)
interfaceMemberDecl =
    (do cd  <- classDecl
        return $ \ms -> MemberClassDecl (cd ms)) <|>
    (do id  <- try annInterfaceDecl <|> try interfaceDecl
        return $ \ms -> MemberInterfaceDecl (id ms)) <|>
    try fieldDecl <|>
    absMethodDecl

absMethodDecl :: P (Mod MemberDecl)
absMethodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    semiColon
    return $ \ms -> MethodDecl ms tps rt id fps thr def (MethodBody Nothing)

defaultValue :: P Exp
defaultValue = tok KW_Default >> exp

throws :: P [RefType]
throws = tok KW_Throws >> refTypeList

-- Formal parameters

formalParams :: P [FormalParam]
formalParams = parens $ do
    fps <- seplist formalParam comma
    if validateFPs fps
     then return fps
     else fail "Only the last formal parameter may be of variable arity"
  where validateFPs :: [FormalParam] -> Bool
        validateFPs [] = True
        validateFPs [_] = True
        validateFPs (FormalParam _ _ b _ :xs) = not b

formalParam :: P FormalParam
formalParam = do
    ms  <- list modifier
    typ <- ttype
    var <- bopt ellipsis
    vid <- varDeclId
    return $ FormalParam ms typ var vid

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P Modifier
modifier =
        tok KW_Public      >> return Public
    <|> tok KW_Protected   >> return Protected
    <|> tok KW_Private     >> return Private
    <|> tok KW_Abstract    >> return Abstract
    <|> tok KW_Static      >> return Static
    <|> tok KW_Strictfp    >> return StrictFP
    <|> tok KW_Final       >> return Final
    <|> tok KW_Native      >> return Native
    <|> tok KW_Transient   >> return Transient
    <|> tok KW_Volatile    >> return Volatile
    <|> tok KW_Synchronized >> return Synchronized_
    <|> Annotation <$> annotation

annotation :: P Annotation
annotation = flip ($) <$ tok Op_AtSign <*> name <*> (
               try (flip NormalAnnotation <$> parens evlist)
           <|> try (flip SingleElementAnnotation <$> parens elementValue)
           <|> try (MarkerAnnotation <$ return ())
        )

evlist :: P [(Ident, ElementValue)]
evlist = seplist1 elementValuePair comma

elementValuePair :: P (Ident, ElementValue)
elementValuePair = (,) <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P ElementValue
elementValue =
    EVVal <$> (    InitArray <$> arrayInit
               <|> InitExp   <$> condExp )
    <|> EVAnn <$> annotation


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P [VarDecl]
varDecls = seplist1 varDecl comma

varDecl :: P VarDecl
varDecl = do
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInit
    return $ VarDecl vid mvi

varDeclId :: P VarDeclId
varDeclId = do
    id  <- ident
    abs <- list arrBrackets
    return $ foldl (\f _ -> VarDeclArray . f) VarId abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P ([Modifier], Type, [VarDecl])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInit :: P VarInit
varInit =
    InitArray <$> arrayInit <|>
    InitExp   <$> exp

arrayInit :: P ArrayInit
arrayInit = braces $ do
    vis <- seplist varInit comma
    opt comma
    return $ ArrayInit vis

----------------------------------------------------------------------------
-- Statements

block :: P Block
block = braces $ Block <$> list blockStmt

blockStmt :: P BlockStmt
blockStmt =
    (try $ do
        ms  <- list modifier
        cd  <- classDecl
        return $ LocalClass (cd ms)) <|>
    (try $ do
        (m,t,vds) <- endSemi $ localVarDecl
        return $ LocalVars m t vds) <|>
    BlockStmt <$> stmt

stmt :: P Stmt
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens exp
        (try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ IfThenElse e th el) <|>
           (do th <- stmt
               return $ IfThen e th)
    whileStmt = do
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $
            (try $ do
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ BasicFor fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ EnhancedFor ms t i e)
        s <- stmt
        return $ f s
    labeledStmt = try $ do
        lbl <- ident
        colon
        s   <- stmt
        return $ Labeled lbl s

stmtNSI :: P Stmt
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElse e th el
    whileStmt = do
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ BasicFor fi e fu)
            <|> (do
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ EnhancedFor ms t i e)
        s <- stmtNSI
        return $ f s
    labeledStmt = try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ Labeled i s

stmtNoTrail :: P Stmt
stmtNoTrail =
    -- empty statement
    const Empty <$> semiColon <|>
    -- inner block
    StmtBlock <$> block <|>
    -- assertions
    (endSemi $ do
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ Assert e me2) <|>
    -- switch stmts
    (do tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ Switch e sb) <|>
    -- do-while loops
    (endSemi $ do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
        return $ Do s e) <|>
    -- break
    (endSemi $ do
        tok KW_Break
        mi <- opt ident
        return $ Break mi) <|>
    -- continue
    (endSemi $ do
        tok KW_Continue
        mi <- opt ident
        return $ Continue mi) <|>
    -- return
    (endSemi $ do
        tok KW_Return
        me <- opt exp
        return $ Return me) <|>
    -- synchronized
    (do tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ Synchronized e b) <|>
    -- throw
    (endSemi $ do
        tok KW_Throw
        e <- exp
        return $ Throw e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ Try b c mf) <|>
    -- expressions as stmts
    ExpStmt <$> endSemi stmtExp

-- For loops

forInit :: P ForInit
forInit = (do
    try (do (m,t,vds) <- localVarDecl
            return $ ForLocalVars m t vds)) <|>
    (seplist1 stmtExp comma >>= return . ForInitExps)

forUp :: P [Exp]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlock]
switchBlock = braces $ list switchStmt

switchStmt :: P SwitchBlock
switchStmt = do
    lbl <- switchLabel
    bss <- list blockStmt
    return $ SwitchBlock lbl bss

switchLabel :: P SwitchLabel
switchLabel = (tok KW_Default >> colon >> return Default) <|>
    (do tok KW_Case
        e <- exp
        colon
        return $ SwitchCase e)

-- Try-catch clauses

catch :: P Catch
catch = do
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ Catch fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P Exp
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: P Exp
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P Exp
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: P Exp
assignment = do
    lh <- lhs
    op <- assignOp
    e  <- assignExp
    return $ Assign lh op e

lhs :: P Lhs
lhs = try (FieldLhs <$> fieldAccess)
    <|> try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name



exp :: P Exp
exp = assignExp

assignExp :: P Exp
assignExp = try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P Exp
condExp = do
    ie <- infixExp
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (Exp -> Exp)
condExpSuffix = do
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    return $ \ce -> Cond ce th el

infixExp :: P Exp
infixExp = infixExpWithOperators infixOperators

-- See Note [Parsing operators]
infixExpWithOperators :: [P Op] -> P Exp
infixExpWithOperators [] = unaryExp
infixExpWithOperators (op : ops) = do
    ue <- infixExpWithOperators ops
    ies <- list (infixExpSuffix op ops)
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P Op -> [P Op] -> P (Exp -> Exp)
infixExpSuffix infixOp ops =
    (do op <- infixOp
        e2 <- infixExpWithOperators ops
        return $ \e1 -> BinOp e1 op e2) <|>

    -- FIXME 'instanceof' should have the same precedence as relational operators
    (do tok KW_Instanceof
        t  <- refType
        return $ \e1 -> InstanceOf e1 t)

unaryExp :: P Exp
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (do
        t <- parens ttype
        e <- unaryExp
        return $ Cast t e) <|>
    postfixExp

postfixExpNES :: P Exp
postfixExpNES = primary

postfixExp :: P Exp
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: P Exp
primary = primaryNPS |>> primarySuffix

primaryNPS :: P Exp
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P Exp
primaryNoNewArrayNPS =
    Lit <$> literal <|>
    const This <$> tok KW_This <|>
    parens exp <|>
    -- TODO: These two following should probably be merged more
    (try $ do
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLit rt) <|>
    (try $ do
        n <- name
        period >> tok KW_This
        return $ ThisClass n) <|>
    try instanceCreationNPS <|>
    try (MethodInv <$> methodInvocationNPS) <|>
    try (FieldAccess <$> fieldAccessNPS) <|>
    try (ExpName <$> name) <|>
    ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp -> Exp)
primarySuffix = try instanceCreationSuffix <|>
    try ((ArrayAccess .) <$> arrayAccessSuffix) <|>
    try ((MethodInv .) <$> methodInvocationSuffix) <|>
    (FieldAccess .) <$> fieldAccessSuffix


instanceCreationNPS :: P Exp
instanceCreationNPS =
    do tok KW_New
       tas <- lopt typeArgs
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBody
       return $ InstanceCreation tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
    (try $ do ct <- classType
              period
              i <- ident
              tok Op_LThan
              tok Op_GThan
              return $ TypeDeclSpecifierWithDiamond ct i Diamond
    ) <|>
    (try $ do i <- ident
              tok Op_LThan
              tok Op_GThan
              return $ TypeDeclSpecifierUnqualifiedWithDiamond i Diamond
    ) <|>
    (do ct <- classType
        return $ TypeDeclSpecifier ct
    )

instanceCreationSuffix :: P (Exp -> Exp)
instanceCreationSuffix =
     do period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ \p -> QualInstanceCreation p tas i as mcb

instanceCreation :: P Exp
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreation {} -> return icp
     _ -> fail ""


lambdaParams :: P LambdaParams
lambdaParams = try (LambdaSingleParam <$> ident)
               <|> try (parens $ LambdaFormalParams <$> (seplist formalParam comma))
               <|> (parens $ LambdaInferredParams <$> (seplist ident comma))

lambdaExp :: P Exp
lambdaExp = Lambda 
            <$> (lambdaParams <* (tok LambdaArrow))
            <*> ((LambdaBlock <$> (try block))
                 <|> (LambdaExpression <$> exp))

methodRef :: P Exp
methodRef = MethodRef 
            <$> (name <*  (tok MethodRefSep))
            <*> ident

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P FieldAccess
fieldAccessNPS =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i)

fieldAccessSuffix :: P (Exp -> FieldAccess)
fieldAccessSuffix = do
    period
    i <- ident
    return $ \p -> PrimaryFieldAccess p i

fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess fa -> return fa
     _ -> fail ""

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: P MethodInvocation
methodInvocationNPS =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)

methodInvocationSuffix :: P (Exp -> MethodInvocation)
methodInvocationSuffix = do
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> PrimaryMethodCall p [] i as

methodInvocationExp :: P Exp
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInv _ -> return mip
     _ -> fail "") <|>
     (MethodInv <$> methodInvocationNPS)

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P [Argument]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P ArrayIndex
arrayAccessNPS = do
    n <- name
    e <- list1 $ brackets exp
    return $ ArrayIndex (ExpName n) e

arrayAccessSuffix :: P (Exp -> ArrayIndex)
arrayAccessSuffix = do
    e <- list1 $ brackets exp
    return $ \ref -> ArrayIndex ref e

arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccess ain -> return ain
     _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P Exp
arrayCreation = do
    tok KW_New
    t <- nonArrayType
    f <- (try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \t -> ArrayCreateInit t (length ds) ai) <|>
         (do des <- list1 $ try $ brackets exp
             ds  <- list  $ brackets empty
             return $ \t -> ArrayCreate t des (length ds))
    return $ f t

literal :: P Literal
literal =
    javaToken $ \t -> case t of
        IntTok     i -> Just (Int i)
        LongTok    l -> Just (Word l)
        DoubleTok  d -> Just (Double d)
        FloatTok   f -> Just (Float f)
        CharTok    c -> Just (Char c)
        StringTok  s -> Just (String s)
        BoolTok    b -> Just (Boolean b)
        NullTok      -> Just Null
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp -> Exp)
preIncDecOp =
    (tok Op_PPlus >> return PreIncrement) <|>
    (tok Op_MMinus >> return PreDecrement)
prefixOp =
    (tok Op_Bang  >> return PreNot      ) <|>
    (tok Op_Tilde >> return PreBitCompl ) <|>
    (tok Op_Plus  >> return PrePlus     ) <|>
    (tok Op_Minus >> return PreMinus    )
postfixOp =
    (tok Op_PPlus  >> return PostIncrement) <|>
    (tok Op_MMinus >> return PostDecrement)

assignOp :: P AssignOp
assignOp =
    (tok Op_Equal    >> return EqualA   ) <|>
    (tok Op_StarE    >> return MultA    ) <|>
    (tok Op_SlashE   >> return DivA     ) <|>
    (tok Op_PercentE >> return RemA     ) <|>
    (tok Op_PlusE    >> return AddA     ) <|>
    (tok Op_MinusE   >> return SubA     ) <|>
    (tok Op_LShiftE  >> return LShiftA  ) <|>
    (tok Op_RShiftE  >> return RShiftA  ) <|>
    (tok Op_RRShiftE >> return RRShiftA ) <|>
    (tok Op_AndE     >> return AndA     ) <|>
    (tok Op_CaretE   >> return XorA     ) <|>
    (tok Op_OrE      >> return OrA      )

-- The infix operators, grouped by precedence.
-- See Note [Parsing operators]
infixOperators :: [P Op]
infixOperators =
  [ (tok Op_OOr     >> return COr       )

  , (tok Op_AAnd    >> return CAnd      )

  , (tok Op_Or      >> return Or        )

  , (tok Op_Caret   >> return Xor       )

  , (tok Op_And     >> return And       )
  
  , (tok Op_Equals  >> return Equal     ) <|>
    (tok Op_BangE   >> return NotEq     )

  , (tok Op_LThan   >> return LThan     ) <|>
    (tok Op_GThan   >> return GThan     ) <|>                                          
    (tok Op_LThanE  >> return LThanE    ) <|>
    (tok Op_GThanE  >> return GThanE    )

  , (tok Op_LShift  >> return LShift    ) <|>
    (try $ do
       tok Op_GThan   
       tok Op_GThan   
       tok Op_GThan
       return RRShift   ) <|>
           
    (try $ do
       tok Op_GThan 
       tok Op_GThan
       return RShift    )

  , (tok Op_Plus    >> return Add       ) <|>
    (tok Op_Minus   >> return Sub       )

  , (tok Op_Star    >> return Mult      ) <|>
    (tok Op_Slash   >> return Div       ) <|>
    (tok Op_Percent >> return Rem       )
  ]

{-
Note [Parsing operators]
~~~~~~~~~~~~~~~~~~~~~~~~
Each entry in 'infixOperators' generates one level of recursion in
'infixExpWithOperators'. This generates a grammar similar to:

@
  ConditionalOrExpression ::=
    ConditionalAndExpression [ "||" ConditionalOrExpression ]

  ConditionalAndExpression ::=
    InclusiveOrExpression [ "&&" ConditionalAndExpression ]

  // and so on...
@

but the operators associate to the left.

A similar (but more general) pattern can be found in 'Text.Parsec.Expr'.
-}

----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = try (RefType <$> refType) <|> PrimType <$> primType

primType :: P PrimType
primType =
    tok KW_Boolean >> return BooleanT  <|>
    tok KW_Byte    >> return ByteT     <|>
    tok KW_Short   >> return ShortT    <|>
    tok KW_Int     >> return IntT      <|>
    tok KW_Long    >> return LongT     <|>
    tok KW_Char    >> return CharT     <|>
    tok KW_Float   >> return FloatT    <|>
    tok KW_Double  >> return DoubleT

refType :: P RefType
refType =
    (do pt <- primType
        (_:bs) <- list1 arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                        (ArrayType . PrimType) bs pt) <|>
    (do ct <- classType
        bs <- list arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                            ClassRefType bs ct) <?> "refType"

nonArrayType :: P Type
nonArrayType = PrimType <$> primType <|>
    RefType <$> ClassRefType <$> classType

classType :: P ClassType
classType = ClassType <$> seplist1 classTypeSpec period

classTypeSpec :: P (Ident, [TypeArgument])
classTypeSpec = do
    i   <- ident
    tas <- lopt typeArgs
    return (i, tas)

resultType :: P (Maybe Type)
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P [RefType]
refTypeList = seplist1 refType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = do
    i  <- ident
    bs <- lopt bounds
    return $ TypeParam i bs

bounds :: P [RefType]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

typeArgs :: P [TypeArgument]
typeArgs = angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg = tok Op_Query >> Wildcard <$> opt wildcardBound
    <|> ActualType <$> refType

wildcardBound :: P WildcardBound
wildcardBound = tok KW_Extends >> ExtendsBound <$> refType
    <|> tok KW_Super >> SuperBound <$> refType

refTypeArgs :: P [RefType]
refTypeArgs = angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

lopt :: P [a] -> P [a]
lopt p = do mas <- opt p
            case mas of
             Nothing -> return []
             Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
--seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
--seplist1 = sepBy1
seplist1 p sep =
    p >>= \a ->
        try (do sep
                as <- seplist1 p sep
                return (a:as))
        <|> return [a]

startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    return $ foldl (\a s -> s a) x ss

(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c

type Mod a = [Modifier] -> a

parens, braces, brackets, angles :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles   = between (tok Op_LThan)   (tok Op_GThan)

endSemi :: P a -> P a
endSemi p = p >>= \a -> semiColon >> return a

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

------------------------------------------------------------

test = "public class Foo { }"
testFile file = do
  i <- readFile file
  let r = parseCompilationUnit i
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r
