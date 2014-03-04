all: iocaml

PKGS=-package cow,cohttp.lwt,websocket,lwt-zmq,uuidm,yojson

pages.cmo: pages.ml
	ocamlfind c -c -syntax camlp4o \
		-package cow,cow.syntax \
		pages.ml

iocamlserver.cmo: iocamlserver.ml pages.cmo 
	ocamlfind c -c -thread -syntax camlp4o \
		$(PKGS) -package lwt.syntax \
		iocamlserver.ml

iocaml: iocamlserver.cmo pages.cmo
	ocamlfind c -thread -o iocaml -linkpkg \
		$(PKGS) \
		pages.cmo iocamlserver.cmo

clean:
	- rm -f *.cm[io]
	- rm -f iocaml
	- rm *~
	- rm *.json

