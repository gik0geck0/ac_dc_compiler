

all: build

build:
	ghc -Wall parser.hs  main.hs  tokenizer.hs dc_codegen.hs

clean:
	rm main
	rm parser.o main.o tokenizer.o dc_codegen.o parser.hi main.hi tokenizer.hi dc_codegen.hi
