

all: build

build:
	ghc -Wall lexer.hs  main.hs  tokenizer.hs
