module Tokenizer where

type Token = String
type Tokens = [Token]

tokenize :: String -> Tokens
tokenize = words
