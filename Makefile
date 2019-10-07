.PHONY: clean bformat btags brandom b2sexp test bdefs b2ada doc coverage

all: bformat btags brandom btypecheck b2sexp bdefs b2ada

bdefs:
	dune build bdefs/bdefs.exe

b2ada:
	dune build b2ada/b2ada.exe

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
