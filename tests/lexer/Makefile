lexdriver.native: ../../src/scanner.mll epilogue
	cat ../../src/scanner.mll > lexdriver.mll
	cat epilogue >> lexdriver.mll
	ocamlbuild lexdriver.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f lexdriver.native
	rm -rf _build
	rm -rf lexdriver.mll

.PHONY: all
all: clean lexdriver.native
