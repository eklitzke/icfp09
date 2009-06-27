CABAL = cabal

all:
	$(CABAL) configure
	$(CABAL) build

clean:
	-rm -rf dist


run-%-1: all
	dist/build/vm/vm problems/bin$*.obf $*001

run-%-2: all
	dist/build/vm/vm problems/bin$*.obf $*002

run-%-3: all
	dist/build/vm/vm problems/bin$*.obf $*003

run-%-4: all
	dist/build/vm/vm problems/bin$*.obf $*004

run-%: all
	dist/build/vm/vm problems/bin$*.obf $*001

.PHONY: test all
