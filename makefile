.PHONY: iocamlserver.byte iocamlserver.native

all: iocamlserver.byte 
native: iocamlserver.native

filesys.ml:
	ocaml-crunch -o filesys.ml -m plain filesys-1.1

tutorial.ml: tutorial/tutorial.ipynb
	ocaml-crunch -o tutorial.ml -m plain tutorial

iocamlserver.byte: filesys.ml tutorial.ml
	ocamlbuild -use-ocamlfind iocamlserver.byte

iocamlserver.native: filesys.ml tutorial.ml
	ocamlbuild -use-ocamlfind iocamlserver.native

install:
	cp iocamlserver.byte `opam config var bin`/iocaml

clean:
	ocamlbuild -clean
	- rm -f *~
	- rm -f *.json
	-rm -f filesys.ml tutorial.ml

