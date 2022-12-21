.PHONY: test

build:
	dune build

utop:
	dune utop lib

test:
	dune test

play:
	dune exec chess


clean:
	dune clean

doc:
	dune build @doc
