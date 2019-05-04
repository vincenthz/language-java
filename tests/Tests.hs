{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (exp)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import System.Directory
import System.FilePath

import Control.Applicative
import Control.Monad
import Data.List (isSuffixOf)

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty
import qualified Control.Exception as E

instance Arbitrary CompilationUnit where
    arbitrary = CompilationUnit <$> arbitrary <*> arbitrary <*> ((:[]) <$> arbitrary)
instance Arbitrary PackageDecl where
    arbitrary = PackageDecl <$> arbitrary
instance Arbitrary ImportDecl where
    arbitrary = ImportDecl <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary TypeDecl where
    arbitrary = ClassTypeDecl <$> arbitrary
instance Arbitrary ClassDecl where
    arbitrary = ClassDecl <$> pure [] <*> arbitrary <*> pure [] <*> pure Nothing <*> pure [] <*> arbitrary
instance Arbitrary ClassBody where
    arbitrary = ClassBody <$> pure []
instance Arbitrary Name where
    arbitrary = Name <$> (choose (1,3) >>= \len -> replicateM len arbitrary)
instance Arbitrary Ident where
    arbitrary = Ident . unkeyword <$> (choose (1,15) >>= \len -> replicateM len (elements (['a'..'z'] ++ ['A'..'Z'])))
      where unkeyword k
                | k `elem` ["if","do","then","else"] = "x" ++ k
                | otherwise                          = k

----------------------------------------------------------
testJavaDirectory :: FilePath
testJavaDirectory = "tests" </> "java"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

toTestCase expected jFile = testCase (takeBaseName jFile) doTest
  where doTest = do r <- E.try parseOne
                    case r of
                        Left (e :: E.SomeException) -> assertBool ("failure exception: " ++ show e) (not expected)
                        Right (Left perr)           -> assertBool ("failure parse error: " ++ show perr) (not expected)
                        Right (Right p)             -> assertBool ("success: " ++ show p) expected
        parseOne = parser compilationUnit <$> readFile jFile

getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

main = do
    exists <- doesDirectoryExist testJavaDirectory
    when (not exists) $ error "cannot find tests files java directory"
    
    allGoodJavas <- getAllJavaPaths (testJavaDirectory </> "good")
    allBadJavas <- getAllJavaPaths (testJavaDirectory </> "bad")

    defaultMain $ testGroup "java"
        [ testGroup "parsing unit good" (map (toTestCase True) allGoodJavas)
        , testGroup "parsing unit bad" (map (toTestCase False) allBadJavas)
        , testProperty "parsing.generating==id" (\g -> case parser compilationUnit (show $ pretty g) of
                                                            Right g'  -> g == g'
                                                            Left perr -> error (show (pretty g) ++ show perr))
        , testGroup "generating.parsing==id"
          [ testRoundTrip exp "ClassFieldAccess" "Object.super.x"
          , testRoundTrip exp "QualInstanceCreation" "foo.new Bar()"
          , testRoundTrip exp "MethodInvocation" "foo(1 + 2)"
          ]
        , testGroup "operator parsing"
          [ testParseSame exp "precedence 1"
              "1 +  2 * 3"
              "1 + (2 * 3)"
          , testParseSame exp "precedence 2"
              " 1 * 2  +  2 * 3"
              "(1 * 2) + (2 * 3)"
          , testParseSame exp "precedence 3"
              "1 || 2 && 3 | 4 ^ 5 == 6 > 7 >>> 8 - 9 * 10"
              "1 || (2 && (3 | (4 ^ (5 == (6 > (7 >>> (8 - (9 * 10))))))))"
          , testParseSame exp "associativity"
              "  1 - 2  - 3  - 4"
              "((1 - 2) - 3) - 4"
          ]
        ]

testRoundTrip p testName str = testCase testName $
  case parser p str of
    Right syn -> assertEqual "" (prettyPrint syn) str
    Left perr -> error (str ++ show perr)

testParseSame p testName s1 s2 = testCase testName $
  case (parser p s1, parser p s2) of
    (Right e1, Right e2) -> assertEqual "" e1 e2
    result -> error (show result)
