parserdriver.native: ../../src/parser.mly epilogue
	cat ../../src/parser.mly > parserdriver.mly
	cat epilogue >> parserdriver.mly
	ocamlbuild parserdriver.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f parserdriver.native
	rm -rf _build
	rm parserdriver.mly

.PHONY: all
all: clean parser.native
