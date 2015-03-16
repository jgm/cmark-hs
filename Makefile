C_SOURCES=$(wildcard cmark/*.*)
CMARK_DIR=../cmark

build:
	cabal configure && cabal build

prep:
	cabal install --only-dependencies

install:
	cabal install

clean:
	cabal clean

update-c-sources: $(C_SOURCES)

cmark/config.h: $(CMARK_DIR)/build/src/config.h
	cp $< $@

cmark/cmark_export.h: $(CMARK_DIR)/build/src/cmark_export.h
	cp $< $@

cmark/cmark_version.h: $(CMARK_DIR)/build/src/cmark_version.h
	cp $< $@

cmark/%: $(CMARK_DIR)/src/%
	cp $< $@

.PHONY: build prep install test clean bench update-cmark
