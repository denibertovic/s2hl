
.PHONY = build run

build:
	@stack build

run:
	@`pwd`/.stack-work/install/x86_64-linux/lts-3.16/7.10.2/bin/s2hl-exe \
		--statements-dir `pwd`/statements \
		--output-dir `pwd`/out \
		--currency USD \
		--currency HRK

