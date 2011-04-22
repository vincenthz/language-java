module Main where

import Test.HUnit
import Text.Printf (printf)
import Language.Java.Lexer

import qualified LexerQCTest as LQCT

data LexerTest = LexerTest
                    String  -- given input
                    [Token] -- expected result

lexerTests :: [LexerTest]
lexerTests = concat [
    integerTests 0
  , integerTests 1
  , integerTests 23
  , integerTests 52
  , [ LexerTest "\"\\\\\" \"\""
      [StringTok "\\", StringTok ""] ]
  ]

-- | Generates integer lexing tests using decimal, octal, and
-- hexadecimal representations of @int@ and @long@ literals.
integerTests :: Integer -> [LexerTest]
integerTests expectedValue =
  concat [ intAndLongs decimalRep,
           intAndLongs octalRep,
           intAndLongs hexRep ]
  where intAndLongs rep = [ LexerTest rep [intToken],
                            LexerTest (rep ++ "l") [longToken],
                            LexerTest (rep ++ "L") [longToken] ]
        decimalRep = printf "%d"   expectedValue
        octalRep   = printf "0%o"  expectedValue
        hexRep     = printf "0x%x" expectedValue
        intToken   = IntTok  expectedValue
        longToken  = LongTok expectedValue
  
main :: IO ()
main = do
  _ <- runTestTT $ TestList $ map lexerTestToTest lexerTests
  LQCT.run
  return ();
  where
    lexerTestToTest (LexerTest input expected) = TestCase $ do
      let result = [ x | L _ x <- lexer input ]
      assertEqual input expected result
