.PHONY: iocamlserver.byte iocamlserver.native

all: iocamlserver.byte 
native: iocamlserver.native

PKGS=-package cow,cohttp.lwt,websocket,lwt-zmq,uuidm,yojson

iocamlserver.byte:
	ocamlbuild -use-ocamlfind iocamlserver.byte

iocamlserver.native:
	ocamlbuild -use-ocamlfind iocamlserver.native

clean:
	ocamlbuild -clean
	- rm *~
	- rm *.json

