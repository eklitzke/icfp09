CABAL = cabal

all:
	$(CABAL) configure
	$(CABAL) build

clean:
	-rm -rf dist

test:
	echo testing

.PHONY: test all
