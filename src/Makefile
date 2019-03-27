compiler.native: compiler.ml
	ocamlbuild -I passes compiler.native

parser.native: parser.mly ast.mli scanner.mll
	ocamlbuild parser.native

scanner.native: scanner.mll
	ocamlbuild scanner.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f scanner.native
	rm -rf _build

.PHONY: all
all: clean scanner.native
