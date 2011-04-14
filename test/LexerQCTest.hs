{-|

A set of QuickCheck tests for the lexer. These tests are essentially
'unlex-lex' tests where we generate a list of tokens, and then
generate a text that the lexer should convert back into the original
list of tokens.

Converting from a list of tokens to text can be done in several ways,
varying the form of intervening white space and commnents, for
example, or the way in which numeric literals are unlexed.  -}

module LexerQCTest where

import Data.List (intercalate)
import Test.QuickCheck

import Language.Java.Lexer (Token, lexer, L(..))

import TokenGen

run :: IO ()
run = do
  quickCheck $ prop_unlexLex withInterveningSpaces

prop_unlexLex :: ([Token] -> String) -> [Token] -> Bool
prop_unlexLex toText tokens =
  tokens == (map justToken $ lexer $ toText tokens)
    where justToken (L _ t) = t
          
withInterveningSpaces :: [Token] -> String
withInterveningSpaces = intercalate " " . map unlex
