.PHONY: clean printer tags generator test

all: printer tags generator test

printer:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

tags:
	ocamlbuild -Is syntax,lexer,examples/btags -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' btags.native

generator:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bgen.native

test:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' test.native && ./test.native

clean:
	ocamlbuild -clean
