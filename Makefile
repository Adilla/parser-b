.PHONY: clean parser printer tags random test_pp

all: parser printer tags bgen

parser:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bparse.native

printer:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

tags:
	ocamlbuild -Is syntax,lexer,examples/btags -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' btags.native

bgen:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bgen.native

test_pp:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format,qcheck -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' test_pp.native


clean:
	ocamlbuild -clean
