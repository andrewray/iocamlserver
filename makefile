all: iocaml

pages.cmo: pages.ml
	ocamlfind c -c -syntax camlp4o -package cow,cow.syntax pages.ml

iocamlserver.cmo: iocamlserver.ml pages.cmo
	ocamlfind c -c -thread -syntax camlp4o \
		-package lwt.syntax,core,cow,cow.syntax,cohttp.lwt \
		iocamlserver.ml

iocaml: iocamlserver.cmo pages.cmo
	ocamlfind c -thread -o iocaml -linkpkg \
		-package core,cow,cohttp.lwt \
		pages.cmo iocamlserver.cmo

clean:
	- rm -f *.cm[io]
	- rm -f iocaml
	- rm *~

