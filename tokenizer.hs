module Tokenizer where

import Debug.Trace
import Text.Regex.Posix

data Token =
      FTok Float
    | ITok Int
    | Finit
    | Iinit
    | PrintTok
    | PlusTok
    | MinusTok
    | AssignTok
    | IdentifierTok Char
    deriving (Show, Eq)

type Tokens = [Token]

type PartialInt = (String, String)
type PartialFloat = (String, String)

tokenize :: String -> Tokens
tokenize [] = []
tokenize (c:cs) =
    if c == 'f' then Finit:(tokenize cs)
    else if c == 'i' then Iinit:(tokenize cs)
    else if c == 'p' then PrintTok:(tokenize cs)
    else if c == '+' then PlusTok:(tokenize cs)
    else if c == '-' then MinusTok:(tokenize cs)
    else if c == '=' then AssignTok:(tokenize cs)
    else if (c:[]) =~ "^[a-eghj-oq-zA-Z]" then  (IdentifierTok c):tokenize cs
    else if (c:cs) =~ "^[0-9]+\\.[0-9]+" then
        let (ftok, ts) = buildFloat $ fTokenize (c:cs)
        in (FTok ftok):(tokenize ts)
    else if (c:cs) =~ "^[0-9]+" then
        let (itok, ts) = buildInt $ iTokenize (c:cs)
        in (ITok itok):(tokenize ts)
    else if c == ' ' then tokenize cs else trace ("Skipping invalid character: " ++ c:[]) $ tokenize cs

fTokenize :: String -> (String, String)
fTokenize [] = ([],[])
fTokenize (c:[]) = (c:[],[])
fTokenize (c:cs) =
    if (c:[]) =~ "[0-9.]" then floatBuilder c (fTokenize cs)
    else ([], c:cs)

floatBuilder :: Char -> (String, String) -> (String, String)
floatBuilder c (cs, remaining) = (c:cs, remaining)

buildFloat :: PartialFloat -> (Float, String)
buildFloat (fstr, cs) = (read fstr, cs)


iTokenize :: String -> (String, String)
iTokenize [] = ([],[])
iTokenize (c:[]) = (c:[],[])
iTokenize (c:cs) =
    if (c:[]) =~ "[0-9]" then intBuilder c (iTokenize cs)
    else ([], c:cs)

intBuilder :: Char -> (String, String) -> PartialInt
intBuilder c (cs, remaining) = (c:cs, remaining)

buildInt :: PartialInt -> (Int, String)
buildInt (istr, cs) = (read istr, cs)
