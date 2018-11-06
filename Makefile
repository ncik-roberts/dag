clean:
	ocamlbuild -clean

%:
	ocamlbuild -use-ocamlfind -use-menhir $*
