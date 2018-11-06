clean:
	ocamlbuild -clean

%.native:
	ocamlbuild -use-ocamlfind -use-menhir $*.native
