.PHONY: clean bformat btags brandom btype b2sexp test_random test_bformat_with_coverage bdefs b2rust bproject

all: bformat btags brandom btypecheck b2sexp bdefs b2rust bproject

bdefs:
	dune build exe/bdefs.exe

b2rust:
	dune build exe/b2rust.exe

b2sexp:
	dune build exe/b2sexp.exe

bformat:
	dune build exe/bformat.exe

btypecheck:
	dune build exe/btypecheck.exe

btags:
	dune build exe/btags.exe

brandom:
	dune build exe/brandom.exe

bproject:
	dune build exe/bproject.exe

test_random:
	dune build exe/test_random_print_parse.exe && ./_build/default/exe/test_random_print_parse.exe

doc:
	dune build @doc

clean:
	dune clean
