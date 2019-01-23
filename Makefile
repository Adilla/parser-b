.PHONY: clean bformat btags brandom btype b2sexp test_random test_bformat_with_coverage bdefs b2ada b2rust

all: bformat btags brandom btypecheck b2sexp bdefs b2ada b2rust

bdefs:
	dune build exe/bdefs.exe

b2ada:
	dune build exe/b2ada.exe

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

test:
	dune runtest --force

doc:
	dune build @doc

clean:
	dune clean
