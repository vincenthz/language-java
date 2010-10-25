module Language.Java.Parser where

import Language.Java.Lexer ( L(..), Token(..), lexer)
import Language.Java.Syntax

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import Prelude hiding ( exp, catch )
import Data.Maybe ( isJust, catMaybes )
import Control.Monad ( (>=>), liftM )

type P = GenParser (L Token) ()

compilationUnit :: P CompilationUnit
compilationUnit = do
    mpd <- opt packageDecl
    ids <- list importDecl
    tds <- list typeDecl
    return $ CompilationUnit mpd ids (catMaybes tds)

packageDecl :: P PackageDecl
packageDecl = do
    tok KW_Package
    n <- name
    comma
    return $ PackageDecl n
    
importDecl :: P ImportDecl
importDecl = do
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- name
    ds <- bopt $ period >> tok Op_Star
    comma
    return $ ImportDecl st n ds

typeDecl :: P (Maybe TypeDecl)
typeDecl = liftM Just classOrInterfaceDecl <|> (semiColon >> return Nothing)

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = do
    ms <- list modifier
    de <- (do cd <- classDecl
              return $ \ms -> ClassTypeDecl (cd ms))
            <|>
          (do id <- interfaceDecl
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
classBody = liftM ClassBody $ braces classBodyDecls

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
enumBodyDecls = semiColon >> classBodyDecls

classBodyDecls :: P [Decl]
classBodyDecls = list classBodyDecl

interfaceDecl :: P (Mod InterfaceDecl)
interfaceDecl = do
    tok KW_Interface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms -> InterfaceDecl ms id tps exs bod

interfaceBody :: P InterfaceBody
interfaceBody = liftM (InterfaceBody . catMaybes) $ 
    braces $ list interfaceBodyDecl

classBodyDecl :: P Decl
classBodyDecl = (do
    mst <- bopt (tok KW_Static)
    blk <- block
    return $ InitDecl mst blk)
    <|> (do
    ms  <- list modifier
    dec <- memberDecl
    return $ MemberDecl (dec ms))
    
memberDecl :: P (Mod MemberDecl)
memberDecl = 
    (do cd  <- classDecl
        return $ \ms -> MemberClassDecl (cd ms))
      <|>
    (do id  <- interfaceDecl
        return $ \ms -> MemberInterfaceDecl (id ms))
      <|> try fieldDecl <|> try methodDecl <|> constrDecl

fieldDecl :: P (Mod MemberDecl)
fieldDecl = do
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
    return $ \ms -> MethodDecl ms tps rt id fps thr bod

methodBody :: P MethodBody
methodBody = liftM MethodBody $ 
    (semiColon >> return Nothing) <|> (block >>= return . Just)

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
    mec <- opt explConstrInv
    bss <- list blockStmt
    return $ ConstructorBody mec bss
    
explConstrInv :: P ExplConstrInv
explConstrInv = endSemi $
    (try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        return $ ThisInvoke tas as)
    <|> (try $ do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ SuperInvoke tas as)
    <|> (do
        pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        return $ PrimarySuperInvoke pri tas as)

interfaceBodyDecl :: P (Maybe MemberDecl)
interfaceBodyDecl = (semiColon >> return Nothing) <|> do
    ms  <- list modifier
    imd <- interfaceMemberDecl
    return $ Just (imd ms)
    
interfaceMemberDecl :: P (Mod MemberDecl)
interfaceMemberDecl =
    (do cd  <- classDecl
        return $ \ms -> MemberClassDecl (cd ms))
      <|>
    (do id  <- interfaceDecl
        return $ \ms -> MemberInterfaceDecl (id ms))
      <|> try fieldDecl <|> absMethodDecl

absMethodDecl :: P (Mod MemberDecl)
absMethodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    thr <- lopt throws
    semiColon
    return $ \ms -> MethodDecl ms tps rt id fps thr (MethodBody Nothing)

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

modifier :: P Modifier
modifier = 
        (tok KW_Public      >> return Public    )
    <|> (tok KW_Protected   >> return Protected )
    <|> (tok KW_Private     >> return Private   )
    <|> (tok KW_Abstract    >> return Abstract  )
    <|> (tok KW_Static      >> return Static    )
    <|> (tok KW_Strictfp    >> return StrictFP  )
    <|> (tok KW_Final       >> return Final     )
    <|> (tok KW_Native      >> return Native    )
    <|> (tok KW_Transient   >> return Transient )
    <|> (tok KW_Volatile    >> return Volatile  )

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P [VarDecl]
varDecls = seplist1 varDecl comma

varDecl :: P VarDecl
varDecl = do
    vid <- varDeclId
    mvi <- opt varInit
    return $ VarDecl vid mvi

varDeclId :: P VarDeclId
varDeclId = do
    id  <- ident
    abs <- list arrBrackets
    return $ foldr (\_ f -> VarDeclArray . f) VarId abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P ([Modifier], Type, [VarDecl])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInit :: P VarInit
varInit = tok Op_Equal >> 
    ((arrayInit >>= return . InitArray) <|> (exp >>= return . InitExp))

arrayInit :: P ArrayInit
arrayInit = braces $ do
    vis <- seplist varInit comma
    opt comma
    return $ ArrayInit vis

----------------------------------------------------------------------------
-- Statements

block :: P Block
block = braces $ liftM Block (list blockStmt)

blockStmt :: P BlockStmt
blockStmt = (try $ do
    ms  <- list modifier
    cd  <- classDecl
    return $ LocalClass (cd ms))
    <|> (try $ do
    (m,t,vds) <- endSemi $ localVarDecl
    return $ LocalVars m t vds)
    <|> liftM BlockStmt stmt

stmt :: P Stmt
stmt = (do
    -- ifThen and ifThenElse, with a common prefix
    tok KW_If
    e   <- parens exp
    (try $ do
        th <- stmtNSI
        tok KW_Else
        el <- stmt
        return $ IfThenElse e th el)
        <|> (do
        th <- stmt
        return $ IfThen e th))
    <|> (do
    -- while loops
    tok KW_While
    e   <- parens exp
    s   <- stmt
    return $ While e s)
    <|> (do
    -- basic and enhanced for
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
    s <- stmt
    return $ f s)
    <|> (try $ do
    -- labeled statements
    lbl <- ident
    colon
    s   <- stmt
    return $ Labeled lbl s)
    -- the rest
    <|> stmtNoTrail

stmtNoTrail :: P Stmt
stmtNoTrail = 
    -- empty statement
    (semiColon >> return Empty) <|>
    -- inner block
    (block >>= return . StmtBlock) <|>
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
    (stmtExp >>= return . ExpStmt)

stmtNSI :: P Stmt
stmtNSI =
    -- if statements - only full ifThenElse
    (do tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElse e th el) <|>
    -- while loops
    (do tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ While e s) <|>
    -- for loops, both basic and enhanced
    (do tok KW_For
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
        return $ f s) <|>
    -- labeled stmts
    (try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ Labeled i s) <|>
    -- the rest
    stmtNoTrail

-- For loops

forInit :: P ForInit
forInit = (do
    (m,t,vds) <- localVarDecl
    return $ ForLocalVars m t vds) <|>
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
stmtExp = instanceCreation 
    <|> preIncDec
    <|> try postIncDec
    <|> try assignment
    <|> liftM MethodInv methodInvocation

preIncDec :: P Exp
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P Exp
postIncDec = do
    e <- postfixExp
    op <- postfixOp
    return $ op e

assignment :: P Exp
assignment = do
    lh <- lhs
    op <- assignOp
    e  <- assignExp
    return $ Assign lh op e

lhs :: P Lhs
lhs = try (liftM NameLhs name)
    <|> try (liftM FieldLhs fieldAccess)
    <|> liftM (uncurry ArrayLhs) arrayAccess

exp :: P Exp
exp = assignExp

assignExp :: P Exp
assignExp = try assignment <|> condExp

condExp :: P Exp
condExp = (try $ do
    ce <- condExp
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    return $ Cond ce th el) <|> infixExp

infixExp :: P Exp
infixExp = 
    (try $ do
        e1 <- infixExp
        op <- infixOp
        e2 <- unaryExp
        return $ BinOp e1 op e2) <|> 
    (try $ do
        e1 <- infixExp
        tok KW_Instanceof
        t  <- refType
        return $ InstanceOf e1 t) <|>
    unaryExp
    
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

postfixExp :: P Exp
postfixExp = try postIncDec <|>
    try primary <|>
    liftM ExpName name

primary :: P Exp
primary = try arrayCreation <|> primaryNoNewArray

primaryNoNewArray :: P Exp
primaryNoNewArray = liftM Lit literal <|>
    (try $ do 
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLit rt) <|>
    (tok KW_This >> return This) <|>
    (try $ do
        n <- name
        period >> tok KW_This
        return $ ThisClass n) <|>
    (liftM Paren (parens exp)) <|>
    instanceCreation <|>
    liftM FieldAccess (try fieldAccess) <|>
    liftM MethodInv (try methodInvocation) <|>
    liftM (uncurry ArrayAccess) arrayAccess
    

instanceCreation :: P Exp
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

fieldAccess :: P FieldAccess
fieldAccess =
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i) <|>
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i)

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

args :: P [Argument]
args = parens $ seplist exp comma

-- Arrays

arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = liftM ExpName name <|> primaryNoNewArray

arrayCreation :: P Exp
arrayCreation = do
    tok KW_New
    t   <- ttype
    f <- (try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \t -> ArrayCreateInit t (length ds) ai) <|> 
         (do des <- list1 $ brackets exp
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

infixOp :: P Op
infixOp =
    (tok Op_Star    >> return Mult      ) <|>
    (tok Op_Slash   >> return Div       ) <|>
    (tok Op_Percent >> return Rem       ) <|>
    (tok Op_Plus    >> return Add       ) <|>
    (tok Op_Minus   >> return Sub       ) <|>
    (tok Op_LShift  >> return LShift    ) <|>
    (tok Op_RShift  >> return RShift    ) <|>
    (tok Op_RRShift >> return RRShift   ) <|>
    (tok Op_LThan   >> return LThan     ) <|>
    (tok Op_GThan   >> return GThan     ) <|>
    (tok Op_LThanE  >> return LThanE    ) <|>
    (tok Op_GThanE  >> return GThanE    ) <|>
    (tok Op_Equals  >> return Equal     ) <|>
    (tok Op_BangE   >> return NotEq     ) <|>
    (tok Op_And     >> return And       ) <|>
    (tok Op_Caret   >> return Xor       ) <|>
    (tok Op_Or      >> return Or        ) <|>
    (tok Op_AAnd    >> return CAnd      ) <|>
    (tok Op_OOr     >> return COr       )


----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = liftM PrimType primType <|> liftM RefType refType

primType :: P PrimType
primType =
    (tok KW_Boolean >> return BooleanT  ) <|>
    (tok KW_Byte    >> return ByteT     ) <|>
    (tok KW_Short   >> return ShortT    ) <|>
    (tok KW_Int     >> return IntT      ) <|>
    (tok KW_Long    >> return LongT     ) <|>
    (tok KW_Char    >> return CharT     ) <|>
    (tok KW_Float   >> return FloatT    ) <|>
    (tok KW_Double  >> return DoubleT   )

refType :: P RefType
refType = (try $ do
        t <- ttype
        brackets empty
        return $ ArrayType t) <|> liftM ClassRefType classType

classType :: P ClassType
classType = liftM ClassType $ seplist1 classTypeSpec period

classTypeSpec :: P (Ident, [TypeArgument])
classTypeSpec = do
    i   <- ident
    tas <- lopt typeArgs
    return (i, tas)

resultType :: P (Maybe Type)
resultType = (tok KW_Void >> return Nothing) <|> liftM Just ttype

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
typeArg = (tok Op_Query >> liftM Wildcard (opt wildcardBound))
    <|> liftM ActualType refType

wildcardBound :: P WildcardBound
wildcardBound = (tok KW_Extends >> liftM ExtendsBound refType)
    <|> (tok KW_Super >> liftM SuperBound refType)

refTypeArgs :: P [RefType]
refTypeArgs = angles $ list refType

----------------------------------------------------------------------------
-- Names

name :: P Name
name = liftM Name $ seplist1 ident comma

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
seplist = sepBy

seplist1 :: P a -> P sep -> P [a]
seplist1 = sepBy1

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
