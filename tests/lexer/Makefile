lexdriver.native: ../../src/scanner.mll epilogue
	cat ../../src/scanner.mll > driver.mll
	cat epilogue >> driver.mll
	ocamlbuild driver.native

.PHONY: clean
clean:
	ocamlbuild -clean 2>/dev/null
	rm -f driver.native
	rm -rf _build
	rm -rf driver.mll

.PHONY: all
