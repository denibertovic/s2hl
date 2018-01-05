
.PHONY = build run static-build clean release

PROJECT_NAME ?= $(shell grep "^name" s2hl.cabal | cut -d " " -f17)
VERSION ?= $(shell grep "^version:" s2hl.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)
ARCH=$(shell uname -m)

export BINARY_PATH = `stack path --local-install-root`/bin/${PROJECT_NAME}-exe

BINARY_NAME = ${PROJECT_NAME}-exe

build:
	@stack build
	@echo "\nBinary available at:\n"
	@echo ${BINARY_PATH}


run:
	@${BINARY_PATH} \
		--statements-dir `pwd`/statements \
		--output-dir `pwd`/out \
		--currency USD \
		--currency HRK \
		--debug

static-build: clean
	@mkdir -p release/build
	@stack ghc -- \
	app/Main.hs \
	src/S2HL/Lib.hs \
	src/S2HL/Options.hs \
	src/S2HL/Types.hs \
	-static \
	-rtsopts=all \
	-optl-pthread \
	-optl-static \
	-O2 \
	-threaded \
	-odir release/build \
	-hidir release/build \
	-o release/${BINARY_NAME}-${VERSION}-linux-${ARCH}

clean:
	@rm -rf release

release: static-build
	@echo "\n\nRelease available at:\n"
	@echo "STATIC BINARY: `pwd`/release/{BINARY_NAME}-${VERSION}-linux-${ARCH}\n"

