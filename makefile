.PHONY: iocamlserver.byte iocamlserver.native

all: iocamlserver.byte 
native: iocamlserver.native

PKGS=-package cow,cohttp.lwt,websocket,lwt-zmq,uuidm,yojson

filesys.ml:
	ocaml-crunch -o filesys.ml -m plain filesys-1.1

iocamlserver.byte: filesys.ml
	ocamlbuild -use-ocamlfind iocamlserver.byte

iocamlserver.native: filesys.ml
	ocamlbuild -use-ocamlfind iocamlserver.native

clean:
	ocamlbuild -clean
	- rm *~
	- rm *.json
	-rm filesys.ml

