all:
	ocamlbuild -package str mochi.native

clean:
	ocamlbuild -clean
