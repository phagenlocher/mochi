all:
	ocamlbuild -Is ltl,wlang -package str mochi.native

clean:
	ocamlbuild -clean
