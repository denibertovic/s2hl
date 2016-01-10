
.PHONY = build run static-build clean release

VERSION=0.1.0.0
ARCH=$(shell uname -m)

BINARY_NAME=s2hl


build:
	@stack build
	@echo "\nBinary available at:\n"
	@echo "`pwd`/.stack-work/install/${ARCH}-linux/lts-3.16/7.10.2/bin/${BINARY_NAME}-exe"


run:
	@`pwd`/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/${BINARY_NAME}-exe \
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

