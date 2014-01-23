module Lexer where

import Data.Maybe
import Debug.Trace
import Tokenizer
import Text.Regex.Posix

type Parser a = Tokens -> Maybe (a, Tokens)

data Program = Prog Declarations Statements deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Declaration =
      Finit Float
    | Iinit Int deriving Show
data Declarations =
      Declarations Declaration Declarations
    | NoDeclarations deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Statements =
      Statements Statement Statements
    | NoStatements deriving Show
data Statement =
      Assignment Identifier Value Expression
    | Print Identifier deriving Show

-- Is none the best way to do Lambda?
data Expression =
      Expression Operator Value Expression
    | NoExpression deriving Show

data Value =
      SValue Identifier
    | FValue Float
    | IValue Int deriving Show

data Identifier = Identifier String deriving Show

data Operator =
      Plus
    | Minus deriving Show

lexer :: Tokens -> Maybe Statements
-- lexer = fst . program
-- lexer tks = let prog = value tks
--     in if isJust prog then fst $ fromJust prog else SValue $ Identifier "INVALID TOKEN!!!!"
lexer tks = let prog = statements tks
    in if isJust prog then Just $ fst $ fromJust prog else Nothing

{-|
program :: Parser Program
program tkns =
    let (dcls, trd) = fromJust $ declarations tkns
        (exprs, tre) = fromJust $ expressions trd
    in Just (Program dcls exprs, tre)

declarations :: Parser Declarations
declarations tkns = if head tkns != "+" && head tkns != "-"
-}

statements :: Parser Statements
statements [] = Just (NoStatements, [])
statements (t:[]) = Just (NoStatements, t:[])
statements (t:ts) =
    -- Predict if there are more statements
    if t == "print" || (isJust $ identifier (t:[]))
        then let stmt = statement (t:ts)
        in if isJust stmt then
            let moreStmts = statements $ snd $ fromJust stmt
            in if isJust moreStmts
                then Just (Statements (fst $ fromJust stmt) (fst $ fromJust moreStmts), snd $ fromJust moreStmts)
            else trace "Stmts::MoreStatements has Errored" Nothing
        else trace "Stmts::oneStatement has Errored" Nothing
    else
        Just (NoStatements, t:ts)


statement :: Parser Statement
statement [] = trace "Ran out of tokens" Nothing
statement (_:[]) = trace "Statements require more than 1 token" Nothing
statement (t:ts) =
    if t == "print" then
        let ident = identifier ts
        in if isJust ident then Just (Print $ fst $ fromJust ident, snd $ fromJust ident)
            else Nothing
    else
        let saveTo = identifier (t:[])
        in if isJust saveTo && head ts == "=" then
            let val = value $ tail ts
            in if isJust val then
                if length ts > 1 then
                    let expr = expression $ tail $ tail ts
                    in if isJust expr then Just (Assignment (fst $ fromJust saveTo) (fst $ fromJust val) (fst $ fromJust expr), snd $ fromJust expr)
                    else trace "Statement:: Following expression has failed" Nothing
                else trace "Statement:: Not enough tokens for '=' and a value" Nothing
            else trace "Statement:: No value found" Nothing
        else trace "Statement:: Expected an identifier followed by an equals" Nothing

expression :: Parser Expression
expression [] = Just (NoExpression, [])
expression (t:ts) =
    let maybeOper = operator (t:[])
    in if isJust maybeOper
        then let val = value ts
        in if isJust val then
            let rExpr = expression $ snd $ fromJust val
            in if isJust rExpr then Just (Expression (fst $ fromJust maybeOper) (fst $ fromJust val) (fst $ fromJust rExpr) , snd $ fromJust rExpr)
        -- We only error if we DO get an operator: We expect valid things to follow after. At this point, we can't allow a Lambda to capture nothing
            else trace ("Expression:: Recursive Expression Failed for tokens " ++ (show $ snd $ fromJust val)) Nothing
        else trace "Expression:: Expected a Value" Nothing
    else Just (NoExpression, t:ts)

operator :: Parser Operator
operator [] = Nothing
operator (t:ts) =
    if t == "+" then Just (Plus, ts)
    else if t == "-" then Just (Minus, ts)
    else Nothing

value :: Parser Value
value [] = Nothing
value (t:ts) =
    if t =~ "^[0-9]+$" then Just (IValue $ read t, ts)
    else if t =~ "^[+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([eE][+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+))?$" then Just (FValue $ read t, ts)
    else let ident = identifier (t:[])
        in if not $ isNothing ident then Just (SValue $ fst $ fromJust ident, ts) else Nothing

identifier :: Parser Identifier
identifier [] = Nothing
identifier (t:ts) =
    if t =~ "^[a-zA-Z]+$" then Just (Identifier t, ts) else Nothing
