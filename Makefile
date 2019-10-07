.PHONY: clean bformat btags brandom btypecheck b2sexp test bdefs doc coverage

all: bformat btags brandom btypecheck b2sexp bdefs

bdefs:
	dune build exe/bdefs.exe

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

coverage:
	rm -f `find . -name 'bisect*.out'`
	BISECT_ENABLE=YES dune build exe/bformat.exe
	dune runtest --force tests 2> /dev/null
	bisect-ppx-report -I _build/default/ -html _coverage/ `find . -name 'bisect*.out'`
