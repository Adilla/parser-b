.PHONY: clean bformat btags brandom btype b2sexp test_random

OCB_OPT=-Is src,tools -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

all: bformat btags brandom btypecheck #b2sexp

bformat:
	ocamlbuild $(OCB_OPT) bformat.native

#b2sexp:
#	ocamlbuild $(OCB_OPT) b2sexp.native

#	BISECT_COVERAGE=YES ocamlbuild -Is lexer,examples -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

btypecheck:
	ocamlbuild $(OCB_OPT) btypecheck.native

btags:
	ocamlbuild $(OCB_OPT) btags.native

brandom:
	ocamlbuild $(OCB_OPT) brandom.native

test_random:
	ocamlbuild $(OCB_OPT) test_random_print_parse.native && ./test_random_print_parse.native

test_bformat_with_coverage:
	BISECT_COVERAGE=YES ocamlbuild $(OCB_OPT) bformat.native
	./bformat.native tests/* > /dev/null
	bisect-ppx-report -I _build/ -html coverage/ `find . -name 'bisect*.out'`

clean:
	rm -f `find . -name 'bisect*.out'`
	ocamlbuild -clean
