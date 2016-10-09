.PHONY: all

all: format lint

sources = src/*.hs src/Analyze/*.hs test/*.hs

lint:
	stack exec hlint -- $(sources)

format:
	stack exec stylish-haskell -- -i $(sources)

deps:
	stack install hlint stylish-haskell
