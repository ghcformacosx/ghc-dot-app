STACK_VER=0.1.2.0
STACK_URL=https://github.com/commercialhaskell/stack/releases/download/v$(STACK_VER)/stack-$(STACK_VER)-x86_64-osx.gz
STACK_DL=./dist/download/stack-$(STACK_VER).gz
STACK_BIN=./dist/unpack/stack-$(STACK_VER)
STACK=$(shell which stack || echo $(STACK_BIN))

all: ghc

GHC.app: ghc
	(cd GHC; xcodebuild)

ghc: $(STACK)
	$(STACK) build
	$(STACK) exec main

$(STACK_BIN):
	mkdir -p ./dist/download ./dist/unpack
	curl -s -L -o $(STACK_DL) $(STACK_URL)
	gzcat $(STACK_DL) > $(STACK_BIN).tmp
	chmod +x $(STACK_BIN).tmp
	mv $(STACK_BIN).tmp $(STACK_BIN)

clean:
	rm -rf dist GHC/build

.PHONY: all clean ghc GHC.app
