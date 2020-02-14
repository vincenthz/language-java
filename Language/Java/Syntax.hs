{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor, PatternSynonyms, TemplateHaskell, ViewPatterns, TupleSections #-}
module Language.Java.Syntax
    ( CompilationUnitA(..)
    , PackageDecl(..)
    , ImportDecl(..)
    , TypeDeclA(..)
    , ClassDeclA(..)
    , ClassBodyA(..)
    , EnumBodyA(..)
    , EnumConstantA(..)
    , InterfaceDeclA(..)
    , InterfaceBodyA(..)
    , InterfaceKind(..)
    , DeclA(..)
    , MemberDeclA(..)
    , DefaultValueA(..)
    , VarDeclA(..)
    , VarDeclId(..)
    , VarInitA(..)
    , FormalParamA(..)
    , MethodBodyA(..)
    , ConstructorBodyA(..)
    , ExplConstrInvA(..)
    , ModifierA(..)
    , AnnotationA(..)
    , desugarAnnotation
    , desugarAnnotation'
    , ElementValueA(..)
    , BlockA(..)
    , BlockStmtA(..)
    , StmtA(..)
    , CatchA(..)
    , SwitchBlockA(..)
    , SwitchLabelA(..)
    , ForInitA(..)
    , ExceptionType
    , ArgumentA
    , ExpA(..)
    , LhsA(..)
    , ArrayIndexA(..)
    , FieldAccessA(..)
    , LambdaParamsA(..)
    , LambdaExpressionA(..)
    , ArrayInitA(..)
    , MethodInvocationA(..)
     -- ** Unannotated aliases and patterns for backwards compatibility
    , CompilationUnit
    , pattern CompilationUnit
    , TypeDecl
    , pattern ClassTypeDecl
    , pattern InterfaceTypeDecl
    , ClassDecl
    , pattern ClassDecl
    , pattern EnumDecl
    , ClassBody
    , pattern ClassBody
    , EnumBody
    , pattern EnumBody
    , EnumConstant,
      pattern EnumConstant
    , InterfaceDecl
    , pattern InterfaceDecl
    , InterfaceBody
    , pattern InterfaceBody
    , Decl
    , pattern MemberDecl
    , pattern InitDecl
    , MemberDecl
    , pattern FieldDecl
    , pattern MethodDecl
    , pattern ConstructorDecl
    , pattern MemberClassDecl
    , pattern MemberInterfaceDecl
    , DefaultValue
    , pattern None
    , pattern Single
    , pattern Array
    , VarDecl
    , pattern VarDecl
    , VarInit
    , pattern InitExp
    , pattern InitArray
    , FormalParam
    , pattern FormalParam
    , MethodBody
    , pattern MethodBody
    , ConstructorBody
    , pattern ConstructorBody
    , ExplConstrInv
    , pattern ThisInvoke
    , pattern SuperInvoke
    , pattern PrimarySuperInvoke
    , Modifier
    , pattern Annotation
    , Annotation
    , ElementValue
    , pattern EVVal
    , pattern EVAnn
    , Block
    , pattern Block
    , BlockStmt
    , pattern BlockStmt
    , pattern LocalClass
    , pattern LocalVars
    , Stmt
    , pattern StmtBlock
    , pattern IfThen
    , pattern IfThenElse
    , pattern While
    , pattern BasicFor
    , pattern EnhancedFor
    , pattern Empty
    , pattern ExpStmt
    , pattern Assert
    , pattern Switch
    , pattern Do
    , pattern Break
    , pattern Continue
    , pattern Return
    , pattern Synchronized
    , pattern Throw
    , pattern Try
    , pattern Labeled
    , Catch
    , pattern Catch
    , SwitchBlock
    , pattern SwitchBlock
    , SwitchLabel
    , ForInit
    , pattern ForLocalVars
    , pattern ForInitExps
    , Argument
    , Exp
    , pattern Lit
    , pattern ClassLit
    , pattern This
    , pattern ThisClass
    , pattern InstanceCreation
    , pattern QualInstanceCreation
    , pattern ArrayCreate
    , pattern ArrayCreateInit
    , pattern FieldAccess
    , pattern MethodInv
    , pattern ArrayAccess
    , pattern ExpName
    , pattern PostIncrement
    , pattern PostDecrement
    , pattern PreIncrement
    , pattern PreDecrement
    , pattern PrePlus
    , pattern PreMinus
    , pattern PreBitCompl
    , pattern PreNot
    , pattern Cast
    , pattern BinOp
    , pattern InstanceOf
    , pattern Cond
    , pattern Assign
    , pattern Lambda
    , pattern MethodRef
    , Lhs
    , pattern NameLhs
    , pattern FieldLhs
    , pattern ArrayLhs
    , ArrayIndex
    , pattern ArrayIndex
    , FieldAccess
    , pattern PrimaryFieldAccess
    , pattern SuperFieldAccess
    , pattern ClassFieldAccess
    , LambdaParams
    , pattern LambdaSingleParam
    , pattern LambdaFormalParams
    , pattern LambdaInferredParams
    , LambdaExpression
    , ArrayInit
    , pattern ArrayInit
    , MethodInvocation
    , module Language.Java.Syntax.Exp
    , module Language.Java.Syntax.Types
    ) where

import Data.Data
import GHC.Generics (Generic)

import Language.Java.Syntax.Types
import Language.Java.Syntax.Exp
import Language.Java.Syntax.Util

-----------------------------------------------------------------------
-- Packages


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnitA a = CompilationUnitA (Maybe (PackageDecl,a)) [(ImportDecl,a)] [TypeDeclA a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type CompilationUnit = CompilationUnitA ()

pattern CompilationUnit :: Maybe PackageDecl -> [ImportDecl] -> [TypeDeclA ()] -> CompilationUnit
pattern CompilationUnit a b c <- CompilationUnitA (fmap fst -> a) (fst . unzip -> b) c ()
    where CompilationUnit a b c = CompilationUnitA (fmap (, ()) a) (fmap (,()) b) c ()

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl = PackageDecl Name
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
    = ImportDecl Bool {- static? -} Name Bool {- .*? -}
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDeclA a
    = ClassTypeDeclA ( ClassDeclA a) a
    | InterfaceTypeDeclA ( InterfaceDeclA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A class declaration specifies a new named reference type.
data ClassDeclA a
    = ClassDeclA [ModifierA a] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBodyA a) a
    | EnumDeclA  [ModifierA a] Ident                             [RefType] (EnumBodyA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBodyA a = ClassBodyA [DeclA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | The body of an enum type may contain enum constants.
data EnumBodyA a = EnumBodyA [EnumConstantA a] [DeclA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | An enum constant defines an instance of the enum type.
data EnumConstantA a = EnumConstantA Ident [ArgumentA a] (Maybe ( ClassBodyA a)) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDeclA a
    = InterfaceDeclA InterfaceKind [ModifierA a] Ident [TypeParam] [RefType] ( InterfaceBodyA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | The body of an interface may declare members of the interface.
newtype InterfaceBodyA a
    = InterfaceBodyA [MemberDeclA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data DeclA a
    = MemberDeclA ( MemberDeclA a) a
    | InitDeclA Bool ( BlockA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDeclA a
    -- | The variables of a class type are introduced by field declarations.
    = FieldDeclA [ModifierA a] Type [VarDeclA a] a
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDeclA      [ModifierA a] [TypeParam] (Maybe Type) Ident [FormalParamA a] [ExceptionType] (DefaultValueA a) ( MethodBodyA a) a
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDeclA [ModifierA a] [TypeParam]              Ident [FormalParamA a] [ExceptionType] ( ConstructorBodyA a) a
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDeclA ( ClassDeclA a) a
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDeclA ( InterfaceDeclA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

data DefaultValueA a
    = NoneA
    | SingleA (ExpA a)
    | ArrayA [ExpA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A declaration of a variable, which may be explicitly initialized.
data VarDeclA a
    = VarDeclA VarDeclId (Maybe ( VarInitA a)) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
    = VarId Ident
    | VarDeclArray VarDeclId
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | Explicit initializer for a variable declaration.
data VarInitA a
    = InitExpA ( ExpA a) a
    | InitArrayA ( ArrayInitA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)


-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParamA a = FormalParamA [ModifierA a] Type Bool VarDeclId a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBodyA a = MethodBodyA (Maybe ( BlockA a))
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBodyA a = ConstructorBodyA (Maybe (ExplConstrInvA a, a)) [BlockStmtA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type ConstructorBody = ConstructorBodyA ()

pattern ConstructorBody :: Maybe (ExplConstrInvA ()) -> [BlockStmtA ()] -> ConstructorBody
pattern ConstructorBody a b <- ConstructorBodyA (fmap fst -> a) b
  where ConstructorBody a b = ConstructorBodyA (fmap (,()) a) b

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInvA a
    = ThisInvokeA             [RefType] [ArgumentA a]
    | SuperInvokeA            [RefType] [ArgumentA a]
    | PrimarySuperInvokeA ( ExpA a) [RefType] [ArgumentA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)


-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data ModifierA a
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
    | AnnotationA ( AnnotationA a)
    | Synchronized_
  deriving (Eq,Read,Typeable,Generic,Data, Functor)

type Modifier = ModifierA ()

pattern Annotation :: Annotation -> Modifier
pattern Annotation a = AnnotationA a

instance Show a => Show ( ModifierA a) where
   show Public = "public"
   show Private = "private"
   show Protected = "protected"
   show Abstract = "abstract"
   show Final = "final"
   show Static = "static"
   show StrictFP = "strictfp"
   show Transient = "transient"
   show Volatile = "volatile"
   show Native = "native"
   show (AnnotationA a) = show a
   show Synchronized_ = "synchronized"

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data AnnotationA a = NormalAnnotation        { annName :: Name -- Not type because not type generics not allowed
                                            , annKV   :: [(Ident, ElementValueA a)] }
                  | SingleElementAnnotation { annName :: Name
                                            , annValue:: ElementValueA a }
                  | MarkerAnnotation        { annName :: Name }
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type Annotation = AnnotationA ()

desugarAnnotation (MarkerAnnotation n)          = (n, [])
desugarAnnotation (SingleElementAnnotation n e) = (n, [(Ident "value", e)])
desugarAnnotation (NormalAnnotation n kv)       = (n, kv)
desugarAnnotation' = uncurry NormalAnnotation . desugarAnnotation

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValueA a = EVValA (VarInitA a)
                     | EVAnnA (AnnotationA a)
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
newtype BlockA a = BlockA [BlockStmtA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmtA a
    = BlockStmtA ( StmtA a) a
    | LocalClassA ( ClassDeclA a)
    | LocalVarsA [ModifierA a] Type [VarDeclA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A Java statement.
data StmtA a
    -- | A statement can be a nested block.
    = StmtBlockA ( BlockA a) a
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThenA ( ExpA a) ( StmtA a) a
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElseA ( ExpA a) ( StmtA a) ( StmtA a) a
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | WhileA ( ExpA a) ( StmtA a) a
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicForA (Maybe (ForInitA a)) (Maybe ( ExpA a)) (Maybe [ExpA a]) ( StmtA a) a
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedForA [ModifierA a] Type Ident ( ExpA a) ( StmtA a) a
    -- | An empty statement does nothing.
    | EmptyA a
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmtA ( ExpA a) a
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | AssertA ( ExpA a) (Maybe ( ExpA a )) a
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | SwitchA ( ExpA a) [SwitchBlockA a] a
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | DoA ( StmtA a) ( ExpA a) a
    -- | A @break@ statement transfers control out of an enclosing statement.
    | BreakA (Maybe Ident) a
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | ContinueA (Maybe Ident) a
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | ReturnA (Maybe ( ExpA a)) a
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | SynchronizedA ( ExpA a) ( BlockA a) a
    -- | A @throw@ statement causes an exception to be thrown.
    | ThrowA ( ExpA a) a
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | TryA ( BlockA a) [CatchA a] (Maybe {- finally -} ( BlockA a)) a
    -- | Statements may have label prefixes.
    | LabeledA Ident ( StmtA a) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data CatchA a = CatchA ( FormalParamA a) ( BlockA a)
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlockA a
    = SwitchBlockA ( SwitchLabelA a) [BlockStmtA a] a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A label within a @switch@ statement.
data SwitchLabelA a
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase (ExpA a)
    | Default
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type SwitchLabel = SwitchLabelA ()

-- | Initialization code for a basic @for@ statement.
data ForInitA a
    = ForLocalVarsA [ModifierA a] Type [VarDeclA a]
    | ForInitExpsA [ExpA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable

-- | Arguments to methods and constructors are expressions.
type ArgumentA = ExpA

-- | A Java expression.
data ExpA a
    -- | A literal denotes a fixed, unchanging value.
    = LitA Literal a
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLitA (Maybe Type) a
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | ThisA a
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClassA Name a
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreationA [TypeArgument] TypeDeclSpecifier [ArgumentA a] (Maybe ( ClassBodyA a)) a
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreationA ( ExpA a) [TypeArgument] Ident [ArgumentA a] (Maybe ( ClassBodyA a)) a
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreateA Type [ExpA a] Int a
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInitA Type Int ( ArrayInitA a) a
    -- | A field access expression.
    | FieldAccessA ( FieldAccessA a) a
    -- | A method invocation expression.
    | MethodInvA ( MethodInvocationA a) a
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccessA ( ArrayIndexA a) a
{-    | ArrayAccess ExpA ExpA -- Should this be made into a datatype, for consistency and use with Lhs? -}
    -- | An expression name, e.g. a variable.
    | ExpNameA Name a
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrementA ( ExpA a) a
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrementA ( ExpA a) a
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrementA  ( ExpA a) a
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrementA  ( ExpA a) a
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlusA (ExpA a) a
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinusA (ExpA a) a
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitComplA (ExpA a) a
    -- | Logical complementation of boolean values.
    | PreNotA (ExpA a) a
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | CastA Type (ExpA a) a
    -- | The application of a binary operator to two operand expressions.
    | BinOpA ( ExpA a) Op ( ExpA a) a
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOfA ( ExpA a) RefType a
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | CondA (ExpA a) ( ExpA a) ( ExpA a) a
    -- | Assignment of the result of an expression to a variable.
    | AssignA (LhsA a) AssignOp ( ExpA a)
    -- | Lambda expression
    | LambdaA (LambdaParamsA a) ( LambdaExpressionA a) a
    -- | Method reference
    | MethodRefA Name (Maybe Ident ) a
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data LhsA a
    = NameLhsA Name a        -- ^ Assign to a variable
    | FieldLhsA ( FieldAccessA a)  -- ^ Assign through a field access
    | ArrayLhsA ( ArrayIndexA a)   -- ^ Assign to an array
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | Array access
data ArrayIndexA a = ArrayIndexA (ExpA a) [ExpA a] a    -- ^ Index into an array
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccessA a
    = PrimaryFieldAccessA ( ExpA a) Ident a     -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccessA Ident a            -- ^ Accessing a field of the superclass.
    | ClassFieldAccessA Name Ident a       -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)


-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParamsA a
  = LambdaSingleParamA Ident a
  | LambdaFormalParamsA [FormalParamA a]
  | LambdaInferredParamsA [(Ident, a)]
    deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

pattern LambdaInferredParams :: [Ident] -> LambdaParamsA ()
pattern LambdaInferredParams a <- LambdaInferredParamsA (fst . unzip -> a)
  where LambdaInferredParams a = LambdaInferredParamsA (map (, ()) a)

-- | Lambda expression, starting from java 8
data LambdaExpressionA a
    = LambdaExpression ( ExpA a)
    | LambdaBlock ( BlockA a)
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type LambdaExpression = LambdaExpressionA ()

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocationA a
    -- | Invoking a specific named method.
    = MethodCall Name [ArgumentA a]
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall ( ExpA a) [RefType] Ident [ArgumentA a]
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall [RefType] Ident [ArgumentA a]
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall Name [RefType] Ident [ArgumentA a]
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  Name [RefType] Ident [ArgumentA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

type MethodInvocation = MethodInvocationA ()

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInitA a
    = ArrayInitA [VarInitA a]
  deriving (Eq,Show,Read,Typeable,Generic,Data, Functor)

unfunctor ''ArrayInitA
unfunctor ''ClassDeclA
unfunctor ''TypeDeclA
unfunctor ''ClassBodyA
unfunctor ''EnumBodyA
unfunctor ''EnumConstantA
unfunctor ''InterfaceDeclA
unfunctor ''InterfaceBodyA
unfunctor ''DeclA
unfunctor ''MemberDeclA
unfunctor ''VarDeclA
unfunctor ''VarInitA
unfunctor ''FormalParamA
unfunctor ''MethodBodyA
unfunctor ''ExplConstrInvA
unfunctor ''ElementValueA
unfunctor ''BlockA
unfunctor ''BlockStmtA
unfunctor ''StmtA
unfunctor ''CatchA
unfunctor ''SwitchBlockA
unfunctor ''ForInitA
unfunctor ''ExpA
type Argument = Exp
unfunctor ''LhsA
unfunctor ''ArrayIndexA
unfunctor ''FieldAccessA
unfunctor ''LambdaParamsA
unfunctor ''DefaultValueA
