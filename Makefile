C_SOURCES=$(wildcard cbits/*.*)
CMARK_DIR?=../cmark

build:
	cabal configure --enable-tests -v3 && cabal build

test:
	cabal test

prep:
	cabal install --only-dependencies

install:
	cabal install

clean:
	cabal clean

bench:
	cabal configure --enable-benchmarks && \
	cabal install --only-dependencies && \
	cabal build && \
	cabal bench

update-c-sources: $(C_SOURCES)

cbits/config.h: $(CMARK_DIR)/build/src/config.h
	cp $< $@

cbits/cmark_export.h: $(CMARK_DIR)/build/src/cmark_export.h
	cp $< $@

cbits/cmark_version.h: $(CMARK_DIR)/build/src/cmark_version.h
	cp $< $@

cbits/%: $(CMARK_DIR)/src/%
	cp $< $@

.PHONY: build prep install test clean bench update-cmark
