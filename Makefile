.PHONY: all

all: format lint

sources = src/Analyze/*.hs test/*.hs

lint:
	stack exec hlint -- -i "Reduce duplication" $(sources)

format:
	stack exec stylish-haskell -- -i $(sources)

deps:
	stack install hlint stylish-haskell

notebook-deps:
	# install frontend from https://github.com/HeinrichApfelmus/hyper-haskell/releases
	stack install hyper hyper-haskell-server hyper-extra

notebook:
	cd notebooks && open -a /Applications/HyperHaskell.app Demo.hhs
