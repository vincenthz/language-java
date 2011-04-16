module Main where

import Test.HUnit
import Language.Java.Lexer

import qualified LexerQCTest as LQCT

data LexerTest = LexerTest
                    String  -- given input
                    [Token] -- expected result

lexerTests :: [LexerTest]
lexerTests = [
    LexerTest "23"
    [IntTok 23]
  , LexerTest "0x1"
    [IntTok 1]
  , LexerTest "0x01"
    [IntTok 1]
  , LexerTest "0x17"
    [IntTok 23]
  , LexerTest "\"\\\\\" \"\""
    [StringTok "\\", StringTok ""]
  ]

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ map lexerTestToTest lexerTests
  LQCT.run
  return ();
  where
    lexerTestToTest (LexerTest input expected) = TestCase $ do
      let result = [ x | L _ x <- lexer input ]
      assertEqual input expected result
