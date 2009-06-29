CABAL = cabal

all:
	$(CABAL) configure
	$(CABAL) build

clean:
	-rm -rf dist


run-1-%: all
	dist/build/vm/vm --session=session.out.1.$* --trace=vis/hs.json --strategy=hohmann --input=problems/bin1.obf --scenario=100$*

run-2-%: all
	dist/build/vm/vm --session=session.out.2.$* --trace=vis/hs.json --strategy=hohmannwait --input=problems/bin2.obf --scenario=200$*

.PHONY: test all
