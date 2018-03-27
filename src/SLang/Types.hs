{-# LANGUAGE NoImplicitPrelude #-}

module SLang.Types where

import           Protolude

import           Data.String (String)


data BExpr
    = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)

-- Binary Boolean operators:
data BBinOp
    = And
    | Or
    deriving (Show)

-- Relational operators:

data RBinOp
    = Greater
    | Less
    deriving (Show)



-- Now we define the types for arithmetic expressions:

data AExpr
    = Var String
    | IntConst Integer
    | Neg AExpr
    | ABinary ABinOp AExpr AExpr
    deriving (Show)

-- arithmetic operators:

data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show)

--- Finally let’s take care of the statements:

data Stmt
    = Seq [Stmt]
    | Assign String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | Skip
    deriving (Show)
