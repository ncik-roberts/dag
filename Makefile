default: main.native
	mv main.native dagc

clean:
	ocamlbuild -clean

%:
	ocamlbuild -use-ocamlfind -use-menhir $*

