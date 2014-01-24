

all: build

build:
	ghc -Wall parser.hs  main.hs  tokenizer.hs
