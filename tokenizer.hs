module Tokenizer where

import Text.Regex.Posix

data Token =
      FTok Float
    | ITok Int
    | Finit
    | Iinit
    | Identifier Char
    deriving Show

type Tokens = [Token]

tokenize :: String -> Tokens
tokenize (c:cs) =
    if c == "f" then Finit
    else if c == "i" then Iinit
    else if c =~ "^[a-eghj-oq-zA-Z]" then  (Identifier c):tokenize cs
