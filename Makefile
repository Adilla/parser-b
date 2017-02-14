.PHONY: clean rcheck 

all:
	ocamlbuild -Is syntax,lexer,examples -use-ocamlfind -pkgs menhirLib -menhir 'menhir --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' bparse.native

clean:
	ocamlbuild -clean
