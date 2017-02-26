.PHONY: clean parser printer tags

all: parser printer tags
parser:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bparse.native

printer:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' pp.native

tags:
	ocamlbuild -Is syntax,lexer,examples/btags -use-ocamlfind -pkgs menhirLib,easy-format -menhir 'menhir --explain --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' btags.native

clean:
	ocamlbuild -clean
