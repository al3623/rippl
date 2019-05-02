default: compiler.native liball.a
	mv compiler.native rippl

compiler.native: compiler.ml codegen.ml
	ocamlbuild -use-ocamlfind -Is passes,utils compiler.native -pkgs unix,llvm,llvm.analysis

parser.native: parser.mly ast.mli scanner.mll
	ocamlbuild parser.native

liball.a: lib.o thunk.o mymap.o
	ar -crs liball.a thunk.o mymap.o lib.o
	ranlib liball.a

lib.o: lib.c lib.h thunk.h

thunk.o: thunk.c thunk.h

mymap.o: mymap.c mymap.h thunk.h lib.h

scanner.native: scanner.mll
	ocamlbuild scanner.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f scanner.native
	rm -rf _build
	rm -rf *.o *.s *.byte exe a.out *.a

.PHONY: all
all: clean default
