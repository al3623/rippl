default: driver.native 

driver.native: driver.ml
	ocamlbuild -use-ocamlfind -Is passes,utils driver.native -pkgs llvm,llvm.analysis

parser.native: parser.mly ast.mli scanner.mll
	ocamlbuild parser.native

scanner.native: scanner.mll
	ocamlbuild scanner.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f driver.native
	rm -rf _build
	rm -rf *.o *.s *.byte exe a.out

.PHONY: all
all: clean default
