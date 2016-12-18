.PHONY: all

all: format lint

sources = src/Analyze/*.hs test/*.hs

lint:
	stack exec hlint -i "Reduce duplication" -- $(sources)

format:
	stack exec stylish-haskell -- -i $(sources)

deps:
	stack install hlint stylish-haskell
