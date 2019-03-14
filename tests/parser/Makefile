parserdriver.native: ../../src/parser.mly epilogue
	cat ../../src/parser.mly > driver.mly
	cat epilogue >> driver.mly
	ocamlbuild driver.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f driver.native
	rm -rf _build
	rm -rf driver.mly

.PHONY: all
all: clean driver.native
