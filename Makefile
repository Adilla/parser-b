.PHONY: clean rcheck 

all:
	ocamlbuild -Is syntax,lexer -use-ocamlfind -pkgs menhirLib -menhir 'menhir --table --unused-token DEFINITIONS --unused-token DEF_FILE --unused-token EQUALEQUAL' parser.native

clean:
	ocamlbuild -clean
