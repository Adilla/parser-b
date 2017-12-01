.PHONY: clean bformat btags brandom btype b2sexp test_random test_bformat_with_coverage

OCB_OPT=-Is src,tools -use-ocamlfind

all: bformat btags brandom btypecheck b2sexp

b2sexp:
	ocamlbuild $(OCB_OPT) b2sexp.native

bformat:
	ocamlbuild $(OCB_OPT) bformat.native

btypecheck:
	ocamlbuild $(OCB_OPT) btypecheck.native

btags:
	ocamlbuild $(OCB_OPT) btags.native

brandom:
	ocamlbuild $(OCB_OPT) brandom.native

test_random:
	ocamlbuild $(OCB_OPT) test_random_print_parse.native && ./test_random_print_parse.native

clean:
	ocamlbuild -clean
