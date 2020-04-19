.PHONY: default build test clean

default: build

build:
	@dune build -p cstubs-demo

test:
	@dune exec -p cstubs-demo hello

clean:
	@dune clean -p cstubs-demo
