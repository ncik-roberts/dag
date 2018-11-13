default: top.native
	mv top.native dagc

clean:
	ocamlbuild -clean

%:
	ocamlbuild -use-ocamlfind -use-menhir $*

