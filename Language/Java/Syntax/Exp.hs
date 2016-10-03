{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Language.Java.Syntax.Exp where

import Data.Data
import GHC.Generics (Generic)

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
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | A binary infix operator.
data Op = Mult | Div | Rem | Add | Sub | LShift | RShift | RRShift
        | LThan | GThan | LThanE | GThanE | Equal | NotEq
        | And | Or | Xor | CAnd | COr
  deriving (Eq,Show,Read,Typeable,Generic,Data)

-- | An assignment operator.
data AssignOp = EqualA | MultA | DivA | RemA | AddA | SubA
              | LShiftA | RShiftA | RRShiftA | AndA | XorA | OrA
  deriving (Eq,Show,Read,Typeable,Generic,Data)
