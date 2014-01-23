module Lexer where

import Data.Maybe
import Tokenizer
import Text.Regex.Posix

type Parser a = Tokens -> Maybe (a, Tokens)

data Program = Prog Declarations Statements deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Declaration = Finit Float | Iinit Int deriving Show
data Declarations = Declarations Declaration Declarations | NoDeclarations deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Statements = Statements Statement Statements | NoStatements deriving Show
data Statement = Assignment Identifier Value Expression | Print Identifier deriving Show

-- Is none the best way to do Lambda?
data Expression = Expression Operator Value Expression | NoExpression deriving Show

data Value = SValue Identifier | FValue Float | IValue Int deriving Show
data Identifier = Identifier String deriving Show

data Operator = Plus | Minus deriving Show

lexer :: Tokens -> Value
-- lexer = fst . program
lexer tks = let prog = value tks
    in if isJust prog then fst $ fromJust prog else SValue $ Identifier "INVALID TOKEN!!!!"

{-|
program :: Parser Program
program tkns =
    let (dcls, trd) = fromJust $ declarations tkns
        (exprs, tre) = fromJust $ expressions trd
    in Just (Program dcls exprs, tre)

declarations :: Parser Declarations
declarations tkns = if head tkns != "+" && head tkns != "-"

statements :: Parser Statements

statement :: Parser Statement

expression :: Parser Expression
-}

value :: Parser Value
value [] = Nothing
value (t:ts) =
    if t =~ "^[0-9]+$" then Just (IValue $ read t, ts)
    else if t =~ "^[+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([eE][+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+))?$" then Just (FValue $ read t, ts)
    else let id = identifier (t:[])
        in if not $ isNothing id then Just (SValue $ fst $ fromJust id, ts) else Nothing

identifier :: Parser Identifier
identifier [] = Nothing
identifier (t:ts) =
    if t =~ "^[a-zA-Z]+$" then Just (Identifier t, ts) else Nothing
