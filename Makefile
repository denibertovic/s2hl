
.PHONY = build run static-build clean release

PROJECT_NAME ?= $(shell grep "^name" s2hl.cabal | cut -d " " -f17)
VERSION ?= $(shell grep "^version:" s2hl.cabal | cut -d " " -f14)
RESOLVER ?= $(shell grep "^resolver:" stack.yaml | cut -d " " -f2)
GHC_VERSION ?= $(shell stack ghc -- --version | cut -d " " -f8)
ARCH=$(shell uname -m)

export BINARY_PATH = `stack path --local-install-root`/bin/${PROJECT_NAME}-exe

BINARY_NAME = ${PROJECT_NAME}-exe

## Build binary
build:
	@stack build
	@echo "\nBinary available at:\n"
	@echo ${BINARY_PATH}

## Helper for local testing
run:
	@${BINARY_PATH} \
		--statements-dir `pwd`/statements \
		--output-dir `pwd`/out \
		--currency USD \
		--currency HRK \
		--debug

## Build static binary
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

## Clean
clean:
	@rm -rf release

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)

