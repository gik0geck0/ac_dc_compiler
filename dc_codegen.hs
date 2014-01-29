module DCGen where

import Parser

genDC :: Program -> String
genDC = genProgram

-- TODO: Do proper float casting and use declarations to determine save type
genProgram :: Program -> String
genProgram (Program _ stmts) =
    genStatements stmts

genStatements :: Statements -> String
genStatements NoStatements = ""
genStatements (Statements stmt moreStmts) =
    (genStatement stmt) ++ (genStatements moreStmts)

genStatement :: Statement -> String
genStatement (Print ident) = (load ident) ++ "p"
genStatement (Assignment (Identifier ident) v expr) =
    (genValue v) ++ (genExpression expr) ++ "s" ++ (ident:[])

genExpression :: Expression -> String
genExpression NoExpression = ""
genExpression (Expression op val moreExp) =
    (genValue val) ++ (genOperator op) ++ (genExpression moreExp)

genValue :: Value -> String
genValue (SValue ident) = genIdentifier ident
genValue (FValue f) = show f
genValue (IValue i) = show i

genOperator :: Operator -> String
genOperator Minus = "-"
genOperator Plus  = "+"

genIdentifier :: Identifier -> String
genIdentifier (Identifier c) = c:[]

-- Instructions to place a register onto the stack without modifying its contents
load :: Identifier -> String
load (Identifier ic) = 'l':ic:[]
