.PHONY: clean bformat btags brandom btypecheck b2sexp test bdefs doc coverage b2rust bproject

all: bformat btags brandom btypecheck b2sexp bdefs b2rust bproject

bdefs:
	dune build bdefs/bdefs.exe

b2rust:
	dune build b2rust/b2rust.exe

bproject:
	dune build bproject/bproject.exe

b2sexp:
	dune build b2sexp/b2sexp.exe

bformat:
	dune build bformat/bformat.exe

btypecheck:
	dune build btypecheck/btypecheck.exe

btags:
	dune build btags/btags.exe

brandom:
	dune build brandom/brandom.exe

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
