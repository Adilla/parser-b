.PHONY: clean parser printer tags random

all: parser printer tags random

parser:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bparse.native

printer:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

tags:
	ocamlbuild -Is syntax,lexer,examples/btags -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' btags.native

random:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' gen_comp.native

clean:
	ocamlbuild -clean
