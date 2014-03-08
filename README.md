# IOCaml Server

[IOCaml](https://github.com/andrewray/iocaml) and [IOCamlJS](https://github.com/andrewray/iocamljs)
provide 'kernels' for running OCaml a REPL in a browser.

These kernels communicate using a protocol defined by the [IPython](http://www.ipython.prg) project
which also provides a python based webserver. 

This project replaces all the python code with an OCaml webserver based on 
[cohttp](https://github.com/avsm/ocaml-cohttp).

# Status

The code implements most features needed for IOCaml.  The notebook and dashboard 
interfaces can be served locally by the webserver.  Features include;

* saving of notebooks (but not checkpointing)
* interrupt+restart of kernels
* multiple notebooks attached to one kernel - this mainly addresses browser refreshes
* browser files are built into the executable

There are some oddities around notebook renaming which need to be addressed.

# Architecture

A http webserver based on cohttp initially provides the dashboard interface.  Upon 
opening of a notebook an IOCaml kernel is started.  The browser is attached to the 
server via 'websockets' and the server is attached to the kernel via 'zmq' sockets.
IOCaml-server bridges the communications between the browser and kernel.

