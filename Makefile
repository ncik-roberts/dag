SHELL=/bin/bash

dagc: main.native
	mv main.native dagc

clean:
	ocamlbuild -clean

%:
	ocamlbuild -use-ocamlfind -use-menhir $*

install:
	sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
	opam init
	eval `opam env`
	opam switch create 4.05.0
	eval `opam env`
	opam install -y core
	opam install -y ppx_jane
