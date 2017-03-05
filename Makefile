.PHONY: clean printer tags generator

all: printer tags generator

printer:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

tags:
	ocamlbuild -Is syntax,lexer,examples/btags -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' btags.native

generator:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bgen.native

clean:
	ocamlbuild -clean
