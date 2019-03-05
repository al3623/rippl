lexdriver.native: ../../src/scanner.mll epilogue
	cat ../../src/scanner.mll > lexdriver.mll
	cat epilogue >> lexdriver.mll
	ocamlbuild lexdriver.native

.PHONY: clean
clean:
	rm -f lexdriver.native
	rm -rf _build

.PHONY: all
all: clean lexdriver.native
