all:
	ocamlbuild driver/main.native

clean:
	ocamlbuild -clean

.PHONY: all clean
