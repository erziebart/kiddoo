.PHONY: default
default: translate graph

translate: translate.ml scanner.cmx parser.cmx ast.cmx
	ocamlopt -c translate.ml
	ocamlfind ocamlopt -linkpkg scanner.cmx parser.cmx ast.cmx translate.cmx -o translate

scanner.cmx: scanner.mll parser.cmx
	ocamllex scanner.mll
	ocamlopt -c scanner.ml

parser.cmx: parser.mly ast.cmx
	ocamlyacc parser.mly
	ocamlopt parser.mli
	ocamlopt -c parser.ml

ast.cmx: ast.ml
	ocamlopt -c ast.ml

graph: graph.c
	gcc -Wall -o graph graph.c `sdl2-config --cflags --libs`

.PHONY: clean
clean:
	rm -f *.cmx *.cmi *.mli *.o test/*.g parser.ml scanner.ml translate graph

test: test.c
	gcc -Wall test.c -o test
