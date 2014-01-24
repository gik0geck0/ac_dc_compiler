

all: build

build:
	ghc -Wall parser.hs  main.hs  tokenizer.hs

clean:
	rm main
	rm parser.o main.o tokenizer.o parser.hi main.hi tokenizer.hi
