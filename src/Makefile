compiler.native: compiler.ml codegen.ml
	ocamlbuild -use-ocamlfind -Is passes,utils compiler.native -pkgs llvm,llvm.analysis

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