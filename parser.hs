module Parser where

import Data.Maybe
import Debug.Trace
import Tokenizer

type Parser a = Tokens -> Maybe (a, Tokens)

data Program = Program Declarations Statements deriving Show

-- Note: It's implied here that there MUST be an initialization. This is because I don't know how to do Empty binding
data Declaration =
      FDcl Identifier
    | IDcl Identifier deriving Show
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

data Identifier = Identifier Char deriving Show

data Operator =
      Plus
    | Minus deriving Show

parse :: Tokens -> Maybe Program
parse tks =
    let prog = program tks
        validProg = validateProgram prog
    in if isJust prog then validProg
    else Nothing
    -- Just $ fst $ fromJust prog else Nothing

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
    in if t == Finit || t == Iinit then
        if isJust decl then
            if isJust moreDecls then Just (Declarations (fst $ fromJust decl) (fst $ fromJust moreDecls), snd $ fromJust moreDecls)
                else trace "Error in a deeper declaration" Nothing
        else trace "Incorrect delaration" Nothing
    else Just (NoDeclarations, (t:ts))

declaration :: Parser Declaration
declaration [] = Nothing
declaration (_:[]) = Nothing
declaration (t:tkns) =
    let ident = identifier tkns
    in if isJust ident then
        if t == Finit then Just (FDcl $ fst $ fromJust ident, snd $ fromJust ident)
        else if t == Iinit then Just (IDcl $ fst $ fromJust ident, snd $ fromJust ident)
        else trace "Invalid type instantiator" Nothing
    -- NOTE: We don't want a trace here. We use the failing from this to tell us that a declaration is not going on.
    else Nothing

statements :: Parser Statements
statements [] = Just (NoStatements, [])
statements (t:[]) = Just (NoStatements, t:[])
statements (t:ts) =
    -- Predict if there are more statements
    if t == PrintTok || (isJust $ identifier (t:[]))
        then
        let stmt = statement (t:ts)
            moreStmts = statements $ snd $ fromJust stmt
        in if isJust stmt then
            if isJust moreStmts
                then Just (Statements (fst $ fromJust stmt) (fst $ fromJust moreStmts), snd $ fromJust moreStmts)
            else trace "Stmts::MoreStatements has Errored" Nothing
        else trace "Stmts::oneStatement has Errored" Nothing
    else Just (NoStatements, t:ts)


statement :: Parser Statement
statement [] = trace "Ran out of tokens" Nothing
statement (_:[]) = trace "Statements require more than 1 token" Nothing
statement (t:ts) =
    case t of
        PrintTok ->
            let ident = identifier ts
            in if isJust ident then Just (Print $ fst $ fromJust ident, snd $ fromJust ident)
                else Nothing
        _        ->
            let saveTo = identifier (t:[])
                expr = expression $ tail $ tail ts
                val = value $ tail ts
            in case head ts of
                AssignTok ->
                    if isJust saveTo then
                        if isJust val then
                            if length ts > 1 then
                                if isJust expr then Just (Assignment (fst $ fromJust saveTo) (fst $ fromJust val) (fst $ fromJust expr), snd $ fromJust expr)
                                else trace "Statement:: Following expression has failed" Nothing
                            else trace "Statement:: Not enough tokens for '=' and a value" Nothing
                        else trace "Statement:: No value found" Nothing
                    else trace "Statement:: No identifier to save to" Nothing
                _  -> trace "Statement:: Expected an identifier followed by an equals" Nothing

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
    case t of
        PlusTok  -> Just (Plus, ts)
        MinusTok -> Just (Minus, ts)
        _        -> Nothing
    -- if t == PlusTok then Just (Plus, ts)
    -- else if t == MinusTok then Just (Minus, ts)
    -- else Nothing

value :: Parser Value
value [] = Nothing
value (t:ts) =
    case t of
        ITok i -> Just (IValue i, ts)
        FTok f -> Just (FValue f, ts)
        _      -> let ident = identifier (t:[])
                    in if not $ isNothing ident then Just (SValue $ fst $ fromJust ident, ts) else Nothing
    -- if t =~ "^[0-9]+$" then Just (IValue $ read t, ts)
    -- else if t =~ "^[+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+)([eE][+-]?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+))?$" then Just (FValue $ read t, ts)
    --else let ident = identifier (t:[])
    --    in if not $ isNothing ident then Just (SValue $ fst $ fromJust ident, ts) else Nothing

identifier :: Parser Identifier
identifier [] = Nothing
identifier (t:ts) =
    -- An identifier must NOT be the single letter: i,f,p, but any identifier that starts with it MUST have more than one character
    case t of
        IdentifierTok it -> Just (Identifier it, ts)
        _               -> trace ("Expected an identifier, but got " ++ show t) Nothing
    -- if t =~ "^[a-eghj-oq-zA-Z]|[a-zA-Z][a-zA-Z]+$" then Just (Identifier t, ts) else Nothing

validateProgram :: Maybe (Program, Tokens) -> Maybe Program
validateProgram mprog =
    let (Program dcls stmts, ts) = fromJust mprog
    in if isJust mprog then
        if ts == [] then
            -- we have a Just program that consumed all the tokens. Good. Now, is it actually valid?
            -- Are all used variables declared?
            let dclist = getDclList dcls
                validStmts = checkDeclarations dclist [] (Just stmts)
            in if isJust validStmts then Just $ Program dcls $ fromJust validStmts
               else Nothing
        else trace "Error: Program did not consume all the tokens" Nothing
    else trace "Cannot validate a Nothing program" Nothing

getDclList :: Declarations -> [Declaration]
getDclList dcls =
    case dcls of
        Declarations adcl moredcls  -> adcl:(getDclList moredcls)
        NoDeclarations               -> []

-- checks that all the variables in the statements are declared
-- Subroutines were injected that ALSO check that variables have been instantiated - something only needed for values and expressions (right side of assignment)
-- Instantiated list will start off empty, and be populated as the function recurses down (recurses sequentially)
checkDeclarations :: [Declaration] -> [Char] -> Maybe Statements -> Maybe Statements
checkDeclarations _ _ Nothing = Nothing
checkDeclarations dclist inslist (Just stmts) =
    case stmts of
        NoStatements -> Just stmts
        Statements stmt moreStmts   ->
            case stmt of
                Print ident ->  if isIdentDeclared dclist ident then
                                    if isIdentInstantiated inslist ident then checkDeclarations dclist inslist $ Just stmts
                                    else trace ("Cannot print uninstantiated identifier: " ++ show ident) Nothing
                                else trace ("Cannot print undeclared identifier: " ++ show ident) Nothing
                Assignment ident val expr ->
                    if isIdentDeclared dclist ident then
                        if isValDeclared dclist inslist val then
                            if isJust $ checkExpressionDeclarations dclist inslist expr then
                                -- ident was isntantiated in this statement. the recursion step on moreStmts should understand that
                                let recStmts = checkDeclarations dclist ((getIdentChar ident):inslist) $ Just moreStmts
                                in if isJust recStmts then Just $ Statements stmt moreStmts
                                else Nothing -- subsequent statement failed
                            else Nothing -- subsequence expression failed
                        else trace (show val ++ " used before instantiation in " ++ show stmt) Nothing
                    else trace (show ident ++ " not declared before use in " ++ show stmt) Nothing

isIdentDeclared :: [Declaration] -> Identifier -> Bool
isIdentDeclared [] _ = False
isIdentDeclared (dcl:ds) ident@(Identifier ic) =
    let dc = getIChar dcl
    in if ic == dc then True
       else isIdentDeclared ds ident

isIdentInstantiated :: [Char] -> Identifier -> Bool
isIdentInstantiated [] _ = False
isIdentInstantiated (i:is) ident@(Identifier ic) =
    if i == ic then True
    else isIdentInstantiated is ident

isValDeclared :: [Declaration] -> [Char] -> Value -> Bool
isValDeclared _ _ (FValue _) = True
isValDeclared _ _ (IValue _) = True
isValDeclared dclist inslist (SValue ident) =
    if isIdentDeclared dclist ident && isIdentInstantiated inslist ident then True
    else False

checkExpressionDeclarations :: [Declaration] -> [Char] -> Expression -> Maybe Expression
checkExpressionDeclarations _ _ NoExpression = Just NoExpression
checkExpressionDeclarations dclist inslist (Expression _ val moreExprs) =
    if isValDeclared dclist inslist val then
        checkExpressionDeclarations dclist inslist moreExprs
    else trace (show val ++ " used before instantiation") Nothing

getIChar :: Declaration -> Char
getIChar (FDcl (Identifier ic)) = ic
getIChar (IDcl (Identifier ic)) = ic

getIdentChar :: Identifier -> Char
getIdentChar (Identifier ic) = ic
