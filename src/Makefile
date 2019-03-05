scanner.native: scanner.mll
	ocamlbuild scanner.native

.PHONY: clean
clean:
	rm -f scanner.native
	rm -rf _build

.PHONY: all
all: clean scanner.native