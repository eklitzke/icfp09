CABAL = cabal

all:
	$(CABAL) configure
	$(CABAL) build

clean:
	-rm -rf dist

run-%: all
	dist/build/vm/vm problems/bin$*.obf

.PHONY: test all
