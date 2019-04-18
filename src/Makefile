default: compiler.native lib.o

compiler.native: compiler.ml codegen.ml
	ocamlbuild -use-ocamlfind -Is passes,utils compiler.native -pkgs llvm,llvm.analysis

parser.native: parser.mly ast.mli scanner.mll
	ocamlbuild parser.native

lib.o: lib.c

scanner.native: scanner.mll
	ocamlbuild scanner.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f scanner.native
	rm -rf _build
	rm -rf *.o *.s *.byte exe a.out

.PHONY: all
all: clean scanner.native
