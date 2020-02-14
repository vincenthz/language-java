{-# LANGUAGE CPP, TupleSections #-}
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

    comma, semiColon, period, colon,

    P

    ) where

import Language.Java.Lexer ( L(..), Token(..), Pos, lexer)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)

import Text.Parsec hiding ( Empty )
import Text.Parsec.Pos

import Prelude hiding ( exp, catch, (>>), (>>=) )
import qualified Prelude as P ( (>>), (>>=) )
import Data.Maybe ( isJust, catMaybes )
import Control.Monad ( ap, void)

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

type Loc = SourcePos

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
parseCompilationUnit = fmap void . parseCompilationUnitA

parseCompilationUnitA :: String -> Either ParseError (CompilationUnitA Loc)
parseCompilationUnitA inp =
    runParser compilationUnit () "" (lexer inp)

parser p = runParser p () "" . lexer

--class Parse a where
--  parse :: String -> a

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P (CompilationUnitA Loc)
compilationUnit = do
    pos <- getPosition
    mpd <- opt packageDecl
    ids <- list importDecl
    tds <- list typeDecl
    eof
    return $ CompilationUnitA ((,pos) <$> mpd) ids (catMaybes tds) pos

packageDecl :: P PackageDecl
packageDecl = do
    tok KW_Package
    n <- name
    semiColon
    return $ PackageDecl n

importDecl :: P ( ImportDecl, Loc)
importDecl = do
    pos <- getPosition
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- name
    ds <- bopt $ period >> tok Op_Star
    semiColon
    return (ImportDecl st n ds, pos)

typeDecl :: P (Maybe ( TypeDeclA Loc ))
typeDecl = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P ( TypeDeclA Loc )
classOrInterfaceDecl = do
    pos <- getPosition
    ms <- list modifier
    (do cd <- classDecl
        return $ ClassTypeDeclA (cd ms) pos) <|>
      (do id <- annInterfaceDecl <|> interfaceDecl
          return $ InterfaceTypeDeclA (id ms) pos)

classDecl :: P (Mod ( ClassDeclA Loc))
classDecl = normalClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod ( ClassDeclA Loc))
normalClassDecl = do
    pos <- getPosition
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extends
    imp <- lopt implements
    bod <- classBody
    return $ \ms -> ClassDeclA ms i tps ((fmap head) mex) imp bod pos

extends :: P [RefType]
extends = tok KW_Extends >> refTypeList

implements :: P [RefType]
implements = tok KW_Implements >> refTypeList

enumClassDecl :: P (Mod ( ClassDeclA Loc))
enumClassDecl = do
    loc <- getPosition
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    return $ \ms -> EnumDeclA ms i imp bod loc

classBody :: P ( ClassBodyA Loc)
classBody = ClassBodyA <$> braces classBodyStatements

enumBody :: P ( EnumBodyA Loc )
enumBody = braces $ do
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ EnumBodyA ecs eds

enumConst :: P ( EnumConstantA Loc )
enumConst = do
    pos <- getPosition
    id  <- ident
    as  <- lopt args
    mcb <- opt classBody
    return $ EnumConstantA id as mcb pos

enumBodyDecls :: P [DeclA Loc]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [DeclA Loc]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod ( InterfaceDeclA Loc ))
annInterfaceDecl = do
    pos <- getPosition
    tok KW_AnnInterface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> InterfaceDeclA InterfaceAnnotation ms id tps exs bod pos

interfaceDecl :: P (Mod ( InterfaceDeclA Loc ))
interfaceDecl = do
    pos <- getPosition
    tok KW_Interface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> InterfaceDeclA InterfaceNormal ms id tps exs bod pos

interfaceBody :: P ( InterfaceBodyA Loc )
interfaceBody = InterfaceBodyA . catMaybes <$>
    braces (list interfaceBodyDecl)

-- Declarations

classBodyStatement :: P (Maybe ( DeclA Loc))
classBodyStatement = do
    pos <- getPosition
    (try $ do
       list1 semiColon
       return Nothing) <|>
      (try $ do
        mst <- bopt (tok KW_Static)
        blk <- block
        return $ Just $ InitDeclA mst blk pos) <|>
      (do ms  <- list modifier
          dec <- memberDecl
          return $ Just $ MemberDeclA (dec ms) pos)

memberDecl :: P (Mod ( MemberDeclA Loc ))
memberDecl = do
    pos <- getPosition
    choice
        [ try $ do
            cd  <- classDecl
            return $ \ms -> MemberClassDeclA (cd ms) pos
        , try $ do
            id  <- try annInterfaceDecl <|> try interfaceDecl
            return $ \ms -> MemberInterfaceDeclA (id ms) pos

        , try fieldDecl
        , try methodDecl
        , constrDecl
        ]

fieldDecl :: P (Mod ( MemberDeclA Loc ))
fieldDecl = endSemi $ do
    pos <- getPosition
    typ <- ttype
    vds <- varDecls
    return $ \ms -> FieldDeclA ms typ vds pos

methodDecl :: P (Mod ( MemberDeclA Loc ))
methodDecl = do
    pos <- getPosition
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBody
    return $ \ms -> MethodDeclA ms tps rt id fps thr Nothing bod pos

methodBody :: P ( MethodBodyA Loc )
methodBody = MethodBodyA <$>
    (const Nothing <$> semiColon <|> Just <$> block)


constrDecl :: P (Mod ( MemberDeclA Loc ))
constrDecl = do
    pos <- getPosition
    tps <- lopt typeParams
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBody
    return $ \ms -> ConstructorDeclA ms tps id fps thr bod pos

constrBody :: P ( ConstructorBodyA Loc )
constrBody = braces $ do
    pos <- getPosition
    mec <- opt (try explConstrInv)
    bss <- list blockStmt
    return $ ConstructorBodyA ((,pos) <$> mec ) bss

explConstrInv :: P ( ExplConstrInvA Loc )
explConstrInv = endSemi $
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ ThisInvokeA tas as) <|>
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ SuperInvokeA tas as) <|>
    (do pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ PrimarySuperInvokeA pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe ( MemberDeclA Loc ))
interfaceBodyDecl = semiColon >> return Nothing <|>
    do ms  <- list modifier
       imd <- interfaceMemberDecl
       return $ Just (imd ms)

interfaceMemberDecl :: P (Mod ( MemberDeclA Loc ))
interfaceMemberDecl = do
    pos <- getPosition
    choice
      [ (\cd ms -> MemberClassDeclA (cd ms) pos) <$> classDecl
      , do id  <- try annInterfaceDecl <|> try interfaceDecl
           return $ \ms -> MemberInterfaceDeclA (id ms) pos
      ]
      <|> try fieldDecl
      <|> absMethodDecl

absMethodDecl :: P (Mod ( MemberDeclA Loc ))
absMethodDecl = do
    pos <- getPosition
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    def <- opt defaultValue
    semiColon
    return $ \ms -> MethodDeclA ms tps rt id fps thr def (MethodBodyA Nothing) pos

defaultValue :: P (ExpA Loc)
defaultValue = tok KW_Default >> exp

throws :: P [RefType]
throws = tok KW_Throws >> refTypeList

-- Formal parameters

formalParams :: P [FormalParamA Loc]
formalParams = parens $ do
    fps <- seplist formalParam comma
    if validateFPs fps
     then return fps
     else fail "Only the last formal parameter may be of variable arity"
  where validateFPs :: [FormalParamA Loc] -> Bool
        validateFPs [] = True
        validateFPs [_] = True
        validateFPs (FormalParamA _ _ b _ _ :xs) = not b

formalParam :: P ( FormalParamA Loc )
formalParam = do
    pos <- getPosition
    ms  <- list modifier
    typ <- ttype
    var <- bopt ellipsis
    vid <- varDeclId
    return $ FormalParamA ms typ var vid pos

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P ( ModifierA Loc)
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
    <|> AnnotationA <$> annotation

annotation :: P ( AnnotationA Loc)
annotation = flip ($) <$ tok Op_AtSign <*> name <*> (
               try (flip NormalAnnotation <$> parens evlist)
           <|> try (flip SingleElementAnnotation <$> parens elementValue)
           <|> try (MarkerAnnotation <$ return ())
        )

evlist :: P [(Ident, ElementValueA Loc)]
evlist = seplist1 elementValuePair comma

elementValuePair :: P (Ident, ElementValueA Loc)
elementValuePair = (,) <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P ( ElementValueA Loc)
elementValue =
    EVValA <$> ( capturePosition $ InitArrayA <$> arrayInit
               <|> InitExpA   <$> condExp )
    <|> EVAnnA <$> annotation


----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P [VarDeclA Loc]
varDecls = seplist1 varDecl comma

varDecl :: P ( VarDeclA Loc)
varDecl = do
    pos <- getPosition
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInit
    return $ VarDeclA vid mvi pos

varDeclId :: P VarDeclId
varDeclId = do
    id  <- ident
    abs <- list arrBrackets
    return $ foldl (\f _ -> VarDeclArray . f) VarId abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P ([ModifierA Loc], Type, [VarDeclA Loc])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInit :: P ( VarInitA Loc )
varInit =
    flip ($) <$> getPosition
    <*>
    (InitArrayA <$> arrayInit <|>
     InitExpA   <$> exp)

arrayInit :: P ( ArrayInitA Loc)
arrayInit = braces $ do
    vis <- seplist varInit comma
    opt comma
    return $ ArrayInitA vis

----------------------------------------------------------------------------
-- Statements

block :: P ( BlockA Loc)
block = braces $ BlockA <$> list blockStmt

blockStmt :: P ( BlockStmtA Loc)
blockStmt =
    (try $ do
        ms  <- list modifier
        cd  <- classDecl
        return $ LocalClassA (cd ms)) <|>
    (try $ do
        (m,t,vds) <- endSemi $ localVarDecl
        return $ LocalVarsA m t vds) <|>
    flip BlockStmtA <$> getPosition <*> stmt

capturePosition :: P (Loc -> a) -> P a
capturePosition f = flip ($) <$> getPosition <*> f

stmt :: P ( StmtA Loc )
stmt =
    capturePosition (choice [ifStmt, whileStmt, forStmt, labeledStmt])
    <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens exp
        (try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ IfThenElseA e th el) <|>
           (do th <- stmt
               return $ IfThenA e th)
    whileStmt = do
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ WhileA e s
    forStmt = do
        tok KW_For
        f <- parens $
            (try $ do
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ BasicForA fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ EnhancedForA ms t i e)
        s <- stmt
        return $ f s
    labeledStmt = try $ do
        lbl <- ident
        colon
        s   <- stmt
        return $ LabeledA lbl s

stmtNSI :: P ( StmtA Loc )
stmtNSI = capturePosition ( choice [ifStmt, whileStmt, forStmt, labeledStmt] ) <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElseA e th el
    whileStmt = do
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ WhileA e s
    forStmt = do
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ BasicForA fi e fu)
            <|> (do
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ EnhancedForA ms t i e)
        s <- stmtNSI
        return $ f s
    labeledStmt = try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ LabeledA i s

stmtNoTrail :: P (StmtA Loc)
stmtNoTrail = capturePosition $
    -- empty statement
    const EmptyA <$> semiColon <|>
    -- inner block
    StmtBlockA <$> block <|>
    -- assertions
    (endSemi $ do
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ AssertA e me2) <|>
    -- switch stmts
    (do tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ SwitchA e sb) <|>
    -- do-while loops
    (endSemi $ do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
        return $ DoA s e) <|>
    -- break
    (endSemi $ do
        tok KW_Break
        mi <- opt ident
        return $ BreakA mi) <|>
    -- continue
    (endSemi $ do
        tok KW_Continue
        mi <- opt ident
        return $ ContinueA mi) <|>
    -- return
    (endSemi $ do
        tok KW_Return
        me <- opt exp
        return $ ReturnA me) <|>
    -- synchronized
    (do tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ SynchronizedA e b) <|>
    -- throw
    (endSemi $ do
        tok KW_Throw
        e <- exp
        return $ ThrowA e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ TryA b c mf) <|>
    -- expressions as stmts
    ExpStmtA <$> endSemi stmtExp

-- For loops

forInit :: P ( ForInitA Loc )
forInit = (do
    try (do (m,t,vds) <- localVarDecl
            return $ ForLocalVarsA m t vds)) <|>
    (seplist1 stmtExp comma >>= return . ForInitExpsA)

forUp :: P [(ExpA Loc)]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlockA Loc]
switchBlock = braces $ list switchStmt

switchStmt :: P ( SwitchBlockA Loc)
switchStmt = capturePosition $ do
    lbl <- switchLabel
    bss <- list blockStmt
    return $ SwitchBlockA lbl bss

switchLabel :: P ( SwitchLabelA Loc )
switchLabel = (tok KW_Default >> colon >> return Default) <|>
    (do tok KW_Case
        e <- exp
        colon
        return $ SwitchCase e)

-- Try-catch clauses

catch :: P ( CatchA Loc )
catch = do
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ CatchA fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P (ExpA SourcePos)
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: P (ExpA SourcePos)
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P (ExpA Loc)
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: P (ExpA Loc)
assignment = do
    lh <- lhs
    op <- assignOp
    e  <- assignExp
    return $ AssignA lh op e

lhs :: P ( LhsA Loc )
lhs =
    try (FieldLhsA <$> fieldAccess)
    <|> try (ArrayLhsA <$> arrayAccess)
    <|> flip NameLhsA <$> getPosition <*> name



exp :: P ( ExpA Loc )
exp = assignExp

assignExp :: P ( ExpA Loc )
assignExp = try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P (ExpA Loc)
condExp = do
    ie <- infixExp
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (ExpA Loc -> ExpA Loc)
condExpSuffix = do
    pos <- getPosition
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    return $ \ce -> CondA ce th el pos

infixExp :: P ( ExpA Loc)
infixExp = do
    ue <- unaryExp
    ies <- list infixExpSuffix
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (ExpA Loc -> ExpA Loc)
infixExpSuffix = capturePosition . fmap flip $
    (do
      op <- infixCombineOp
      ie2 <- infixExp
      return $ \ie1 -> BinOpA ie1 op ie2) <|>
    (do op <- infixOp
        e2 <- unaryExp
        return $ \e1 -> BinOpA e1 op e2) <|>
    (do tok KW_Instanceof
        t  <- refType
        return $ \e1 -> InstanceOfA e1 t)

unaryExp :: P (ExpA Loc)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (capturePosition $ do
        t <- parens ttype
        e <- unaryExp
        return $ CastA t e) <|>
    postfixExp

postfixExpNES :: P (ExpA Loc)
postfixExpNES = -- try postIncDec <|>
    try primary <|>
    capturePosition (ExpNameA <$> name)

postfixExp :: P (ExpA Loc)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: P (ExpA Loc)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (ExpA Loc)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P (ExpA Loc)
primaryNoNewArrayNPS =
  choice
    [ capturePosition $ LitA <$> literal
    , capturePosition $ const ThisA <$> tok KW_This
    , parens exp
    -- TODO: These two following should probably be merged more
    , capturePosition $
      try $ do
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLitA rt
    , capturePosition $
      try $ do
        n <- name
        period >> tok KW_This
        return $ ThisClassA n
    , try instanceCreationNPS
    , capturePosition $ try (MethodInvA <$> methodInvocationNPS)
    , capturePosition $ try (FieldAccessA <$> fieldAccessNPS)
    , capturePosition $ ArrayAccessA <$> arrayAccessNPS
    ]

primarySuffix :: P (ExpA Loc -> ExpA Loc)
primarySuffix =
  try instanceCreationSuffix <|>
  capturePosition
    (flip <$>
     (try ((ArrayAccessA .) <$> arrayAccessSuffix) <|>
      try ((MethodInvA .) <$> methodInvocationSuffix) <|>
      (FieldAccessA .) <$> fieldAccessSuffix))


instanceCreationNPS :: P (ExpA Loc)
instanceCreationNPS = do
    pos <- getPosition
    tok KW_New
    tas <- lopt typeArgs
    tds <- typeDeclSpecifier
    as <- args
    mcb <- opt classBody
    return $ InstanceCreationA tas tds as mcb pos

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

instanceCreationSuffix :: P (ExpA Loc -> ExpA Loc)
instanceCreationSuffix = do
    pos <- getPosition
    period >> tok KW_New
    tas <- lopt typeArgs
    i <- ident
    as <- args
    mcb <- opt classBody
    return $ \p -> QualInstanceCreationA p tas i as mcb pos

instanceCreation :: P (ExpA Loc)
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreationA {} -> return icp
     _ -> fail ""


lambdaParams :: P ( LambdaParamsA Loc)
lambdaParams = try (flip LambdaSingleParamA <$> getPosition <*> ident)
               <|> try (parens $ LambdaFormalParamsA <$> (seplist formalParam comma))
               <|> (parens $ LambdaInferredParamsA <$> (seplist (flip (,) <$> getPosition <*> ident ) comma))

lambdaExp :: P (ExpA Loc)
lambdaExp = capturePosition $ LambdaA
            <$> (lambdaParams <* (tok LambdaArrow))
            <*> ((LambdaBlock <$> (try block))
                 <|> (LambdaExpression <$> exp))

methodRef :: P (ExpA Loc)
methodRef = capturePosition $ MethodRefA
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

fieldAccessNPS :: P ( FieldAccessA Loc )
fieldAccessNPS = capturePosition $
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccessA i) <|>
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccessA n i)

fieldAccessSuffix :: P (ExpA Loc -> FieldAccessA Loc)
fieldAccessSuffix = do
    loc <- getPosition
    period
    i <- ident
    return $ \p -> PrimaryFieldAccessA p i loc

fieldAccess :: P ( FieldAccessA Loc )
fieldAccess = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccessA fa _ -> return fa
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

methodInvocationNPS :: P ( MethodInvocationA Loc )
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

methodInvocationSuffix :: P (ExpA Loc -> MethodInvocationA Loc)
methodInvocationSuffix = do
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> PrimaryMethodCall p [] i as

methodInvocationExp :: P (ExpA Loc)
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInvA _ _ -> return mip
     _ -> fail "") <|>
     (flip MethodInvA <$> getPosition <*> methodInvocationNPS)

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

args :: P [ArgumentA Loc]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P ( ArrayIndexA Loc)
arrayAccessNPS = do
    pos <- getPosition
    n <- name
    e <- list1 $ brackets exp
    return $ ArrayIndexA (ExpNameA n pos) e pos

arrayAccessSuffix :: P (ExpA Loc -> ArrayIndexA Loc)
arrayAccessSuffix = do
    loc <- getPosition
    e <- list1 $ brackets exp
    return $ \ref -> ArrayIndexA ref e loc

arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccessA ain _ -> return ain
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

arrayCreation :: P (ExpA Loc)
arrayCreation = capturePosition $ do
    tok KW_New
    t <- nonArrayType
    f <- (try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \t -> ArrayCreateInitA t (length ds) ai) <|>
         (do des <- list1 $ try $ brackets exp
             ds  <- list  $ brackets empty
             return $ \t -> ArrayCreateA t des (length ds))
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

opFromTokens assocList = do
    pos <- getPosition
    choice $ map (\(tok', op) -> tok tok' >> return (\a -> op a pos)) assocList

preIncDecOp, prefixOp, postfixOp :: P ((ExpA Loc) -> (ExpA Loc))
preIncDecOp =
  opFromTokens [(Op_PPlus, PreIncrementA), (Op_MMinus, PreDecrementA)]
prefixOp =
  opFromTokens
    [ (Op_Bang, PreNotA)
    , (Op_Tilde, PreBitComplA)
    , (Op_Plus, PrePlusA)
    , (Op_Minus, PreMinusA)
    ]
postfixOp =
  opFromTokens [(Op_PPlus, PostIncrementA), (Op_MMinus, PostDecrementA)]
assignOp :: P AssignOp
assignOp =
  choice $
  map
    (\(token, op) -> tok token >> pure op)
    [ (Op_Equal, EqualA)
    , (Op_StarE, MultA)
    , (Op_SlashE, DivA)
    , (Op_PercentE, RemA)
    , (Op_PlusE, AddA)
    , (Op_MinusE, SubA)
    , (Op_LShiftE, LShiftA)
    , (Op_RShiftE, RShiftA)
    , (Op_RRShiftE, RRShiftA)
    , (Op_AndE, AndA)
    , (Op_CaretE, XorA)
    , (Op_OrE, OrA)
    ]

infixCombineOp :: P Op
infixCombineOp =
    (tok Op_And     >> return And       ) <|>
    (tok Op_Caret   >> return Xor       ) <|>
    (tok Op_Or      >> return Or        ) <|>
    (tok Op_AAnd    >> return CAnd      ) <|>
    (tok Op_OOr     >> return COr       )


infixOp :: P Op
infixOp =
    (tok Op_Star    >> return Mult      ) <|>
    (tok Op_Slash   >> return Div       ) <|>
    (tok Op_Percent >> return Rem       ) <|>
    (tok Op_Plus    >> return Add       ) <|>
    (tok Op_Minus   >> return Sub       ) <|>
    (tok Op_LShift  >> return LShift    ) <|>
    (tok Op_LThan   >> return LThan     ) <|>
    (try $ do
       tok Op_GThan
       tok Op_GThan
       tok Op_GThan
       return RRShift   ) <|>

    (try $ do
       tok Op_GThan
       tok Op_GThan
       return RShift    ) <|>

    (tok Op_GThan   >> return GThan     ) <|>
    (tok Op_LThanE  >> return LThanE    ) <|>
    (tok Op_GThanE  >> return GThanE    ) <|>
    (tok Op_Equals  >> return Equal     ) <|>
    (tok Op_BangE   >> return NotEq     )


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

pos2sourcePos :: (Int, Int) -> Loc
pos2sourcePos (l,c) = newPos "" l c

type Mod a = [ModifierA Loc] -> a

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
