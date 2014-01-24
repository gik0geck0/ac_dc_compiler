module Lexer where

import Data.Maybe
import Debug.Trace
import Tokenizer
import Text.Regex.Posix

type Parser a = Tokens -> Maybe (a, Tokens)

data Program = Program Declarations Statements deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Declaration =
      Finit Identifier
    | Iinit Identifier deriving Show
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

parse :: Tokens -> Maybe Program
parse tks = let prog = program tks
    in if isJust prog then Just $ fst $ fromJust prog else Nothing

program :: Parser Program
program tkns =
    let decls = declarations tkns
        stmts = statements $ snd $ fromJust decls
    in if isJust decls then
        if isJust stmts then Just (Program (fst $ fromJust decls) (fst $ fromJust stmts), snd $ fromJust stmts)
        else trace "Error in statements" Nothing
    else trace "Error in declarations." Nothing

declarations :: Parser Declarations
declarations [] = Just (NoDeclarations, [])
declarations (t:[]) = Just (NoDeclarations, t:[])
declarations (t:ts) =
    let decl = declaration (t:ts)
        moreDecls = declarations $ snd $ fromJust decl
    -- Predict if this will be a valid declaration
    in if t == "f" || t == "i" then
        if isJust decl then
            if isJust moreDecls then Just (Declarations (fst $ fromJust decl) (fst $ fromJust moreDecls), snd $ fromJust moreDecls)
                else trace "Error in a deeper declaration" Nothing
        else trace "Incorrect delaration" Nothing
    else Just (NoDeclarations, ts)

declaration :: Parser Declaration
declaration [] = Nothing
declaration (_:[]) = Nothing
declaration (t:tkns) =
    let ident = identifier tkns
    in if isJust ident then
        if t == "f" then Just (Finit $ fst $ fromJust ident, snd $ fromJust ident)
        else if t == "i" then Just (Iinit $ fst $ fromJust ident, snd $ fromJust ident)
        else trace "Invalid type instantiator" Nothing
    -- NOTE: We don't want a trace here. We use the failing from this to tell us that a declaration is not going on.
    else Nothing

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
    if t == "p" then
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
    -- An identifier must NOT be the single letter: i,f,p, but any identifier that starts with it MUST have more than one character
    if t =~ "^[a-eghj-oq-zA-Z]|[a-zA-Z][a-zA-Z]+$" then Just (Identifier t, ts) else Nothing
