.PHONY: default build clean install uninstall #test

default: build

build:
	@dune build

clean:
	@dune clean

install: build
	@dune install

uninstall:
	@dune uninstall
