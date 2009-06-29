CABAL = cabal

all:
	$(CABAL) configure
	$(CABAL) build

clean:
	-rm -rf dist


run-%-1: all
	dist/build/vm/vm --trace=vis/hs.json problems/bin$*.obf $*001
	-mv session.out session.out.$*.1

run-%-2: all
	dist/build/vm/vm --trace=vis/hs.json  problems/bin$*.obf $*002
	-mv session.out session.out.$*.2

run-%-3: all
	dist/build/vm/vm --trace=vis/hs.json  problems/bin$*.obf $*003
	-mv session.out session.out.$*.3

run-%-4: all
	dist/build/vm/vm  --trace=vis/hs.json problems/bin$*.obf $*004
	-mv session.out session.out.$*.4

run-%: all
	dist/build/vm/vm  --trace=vis/hs.json problems/bin$*.obf $*001
	-mv session.out session.out.1.1

.PHONY: test all
