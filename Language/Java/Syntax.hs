module Language.Java.Syntax where

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl = PackageDecl Name
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier. 
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
    = ImportDecl Bool {- static? -} Name Bool {- .*? -}
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl
    = ClassTypeDecl ClassDecl
    | InterfaceTypeDecl InterfaceDecl
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A class declaration specifies a new named reference type.
data ClassDecl
    = ClassDecl [Modifier] Ident [TypeParam] (Maybe RefType) [RefType] ClassBody
    | EnumDecl  [Modifier] Ident                             [RefType] EnumBody
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A class body may contain declarations of members of the class, that is, 
--   fields, classes, interfaces and methods. 
--   A class body may also contain instance initializers, static 
--   initializers, and declarations of constructors for the class.
data ClassBody = ClassBody [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The body of an enum type may contain enum constants.
data EnumBody = EnumBody [EnumConstant] [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An enum constant defines an instance of the enum type.
data EnumConstant = EnumConstant Ident [Argument] (Maybe ClassBody)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An interface declaration introduces a new reference type whose members 
--   are classes, interfaces, constants and abstract methods. This type has 
--   no implementation, but otherwise unrelated classes can implement it by 
--   providing implementations for its abstract methods.
data InterfaceDecl
    = InterfaceDecl [Modifier] Ident [TypeParam] [RefType] InterfaceBody
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The body of an interface may declare members of the interface.
data InterfaceBody
    = InterfaceBody [MemberDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl
    = MemberDecl MemberDecl
    | InitDecl Bool Block
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl [Modifier] Type [VarDecl]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl      [Modifier] [TypeParam] (Maybe Type) Ident [FormalParam] [ExceptionType] MethodBody
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl [Modifier] [TypeParam]              Ident [FormalParam] [ExceptionType] ConstructorBody
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl ClassDecl
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl InterfaceDecl
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
    = VarDecl VarDeclId (Maybe VarInit)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
    = VarId Ident
    | VarDeclArray VarDeclId        
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Explicit initializer for a variable declaration.
data VarInit
    = InitExp Exp
    | InitArray ArrayInit
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam = FormalParam [Modifier] Type Bool VarDeclId
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A method body is either a block of code that implements the method or simply a 
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody = MethodBody (Maybe Block)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody = ConstructorBody (Maybe ExplConstrInv) [BlockStmt]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An explicit constructor invocation invokes another constructor of the 
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately 
--   enclosing instance. 
data ExplConstrInv
    = ThisInvoke             [RefType] [Argument]
    | SuperInvoke            [RefType] [Argument]
    | PrimarySuperInvoke Exp [RefType] [Argument]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
    | StrictFP
    | Transient
    | Volatile
    | Native
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations 
--   and local variable declaration statements within braces.
data Block = Block [BlockStmt]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif



-- | A block statement is either a normal statement, a local 
--   class declaration or a local variable declaration.
data BlockStmt
    = BlockStmt Stmt
    | LocalClass ClassDecl
    | LocalVars [Modifier] Type [VarDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A Java statement.
data Stmt
    -- | A statement can be a nested block.
    = StmtBlock Block
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen Exp Stmt
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse Exp Stmt Stmt
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While Exp Stmt
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some 
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor [Modifier] Type Ident Exp Stmt
    -- | An empty statement does nothing.
    | Empty
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt Exp
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression 
    --   evaluates to false.
    | Assert Exp (Maybe Exp)
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch Exp [SwitchBlock]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do Stmt Exp
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (Maybe Ident)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation 
    --   point of that statement.
    | Continue (Maybe Ident)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (Maybe Exp)
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block, 
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized Exp Block
    -- | A @throw@ statement causes an exception to be thrown. 
    | Throw Exp
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that 
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally 
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly, 
    --   and no matter whether a catch clause is first given control.
    | Try Block [Catch] (Maybe {- finally -} Block)
    -- | Statements may have label prefixes.
    | Labeled Ident Stmt
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be 
--   transferred to the first such catch clause.
data Catch = Catch FormalParam Block
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock
    = SwitchBlock SwitchLabel [BlockStmt]    
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A label within a @switch@ statement. 
data SwitchLabel
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase Exp
    | Default
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Initialization code for a basic @for@ statement.
data ForInit
    = ForLocalVars [Modifier] Type [VarDecl]
    | ForInitExps [Exp]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable


-----------------------------------------------------------------------
-- Expressions

-- | Arguments to methods and constructors are expressions.
type Argument = Exp

-- | A Java expression.
data Exp
    -- | A literal denotes a fixed, unchanging value.
    = Lit Literal
    -- | A class literal, which is an expression consisting of the name of a class, interface, array, 
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (Maybe Type)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass Name
    -- | A parenthesized expression is a primary expression whose type is the type of the contained expression 
    --   and whose value at run time is the value of the contained expression. If the contained expression 
    --   denotes a variable then the parenthesized expression also denotes that variable.
    | Paren Exp
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes 
    --   and their anonymous subclasses.
    | QualInstanceCreation Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate Type [Exp] Int
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit Type Int ArrayInit
    -- | A field access expression.
    | FieldAccess FieldAccess
    -- | A method invocation expression.
    | MethodInv MethodInvocation
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs?
    -- | An expression name, e.g. a variable.
    | ExpName Name
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement Exp
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement Exp
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  Exp
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  Exp
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  Exp
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus Exp
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl Exp
    -- | Logical complementation of boolean values.
    | PreNot  Exp
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another 
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks, 
    --   at run time, that a reference value refers to an object whose class is compatible with a specified 
    --   reference type.
    | Cast  Type Exp
    -- | The application of a binary operator to two operand expressions.
    | BinOp Exp Op Exp
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf Exp RefType
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other 
    --   expressions should be evaluated.
    | Cond Exp Exp Exp
    -- | Assignment of the result of an expression to a variable.
    | Assign Lhs AssignOp Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A literal denotes a fixed, unchanging value.
data Literal
    = Int Integer
    | Word Integer
    | Float Double
    | Double Double
    | Boolean Bool
    | Char Char
    | String String
    | Null
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A binary infix operator.
data Op = Mult | Div | Rem | Add | Sub | LShift | RShift | RRShift
        | LThan | GThan | LThanE | GThanE | Equal | NotEq
        | And | Or | Xor | CAnd | COr
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An assignment operator.
data AssignOp = EqualA | MultA | DivA | RemA | AddA | SubA
              | LShiftA | RShiftA | RRShiftA | AndA | XorA | OrA
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local 
--   variable or a field of the current object or class, or it may be a computed variable, as can result from 
--   a field access or an array access.
data Lhs
    = NameLhs Name          -- ^ Assign to a variable
    | FieldLhs FieldAccess  -- ^ Assign through a field access
    | ArrayLhs Exp Exp      -- ^ Assign to an array
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A field access expression may access a field of an object or array, a reference to which is the value 
--   of either an expression or the special keyword super.
data FieldAccess
    = PrimaryFieldAccess Exp Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess Ident            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess Name Ident       -- ^ Accessing a (static) field of a named class.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation
    -- | Invoking a specific named method. 
    = MethodCall Name [Argument]
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall Exp [RefType] Ident [Argument]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall [RefType] Ident [Argument]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall Name [RefType] Ident [Argument]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  Name [RefType] Ident [Argument]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an 
--   array and providing some initial values
data ArrayInit
    = ArrayInit [VarInit]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-----------------------------------------------------------------------
-- Types


-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType
    | RefType RefType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | There are three kinds of reference types: class types, interface types, and array types. 
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
    = ClassRefType ClassType
    {- | TypeVariable Ident -}
    | ArrayType Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A class or interface type consists of a type declaration specifier, 
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
    = ClassType [(Ident, [TypeArgument])]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
    = Wildcard (Maybe WildcardBound)
    | ActualType RefType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
    = ExtendsBound RefType
    | SuperBound RefType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
    = BooleanT
    | ByteT
    | ShortT
    | IntT
    | LongT
    | CharT
    | FloatT
    | DoubleT
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A class is generic if it declares one or more type variables. These type variables are known 
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name [Ident]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif
