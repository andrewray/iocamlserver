![IOCaml logo](/logos/IOlogo.png "IOCaml logo")

[![Build Status](https://travis-ci.org/andrewray/iocamlserver.svg?branch=master)](https://travis-ci.org/andrewray/iocamlserver)

# IOCaml Server

IOCaml is an OCaml kernel for the 
[IPython notebook](http://ipython.org/notebook.html). 
This provides a REPL within a web browser with a nice user interface 
including markdown based comments/documentation, mathjax formula and 
the possibility of generating all manner of HTML based output media 
from your code.  

See also

* [IOCaml-kernel](https://github.com/andrewray/iocaml)
* [IOCamlJS-kernel](https://github.com/andrewray/iocamljs)
* [IOCaml-server](https://github.com/andrewray/iocamlserver)

This repository hosts the `iocaml-server` package.

This project replaces all the Python code with an OCaml webserver based on 
[cohttp](https://github.com/avsm/ocaml-cohttp).

# Usage

```
$ iocaml [options] [path or file]
```

If started with a path then the dashboard interface will be started
which lists notebooks in the given directory.

If a `.ipynb` file is given then the notebook will be loaded.

By default [iocaml-kernel](https://github.com/andrewray/iocaml) 
is run.  To run a JavaScript kernel use:

```
$ iocaml -js <kernel> [...]
```

where `<kernel>` may currently be either:

* `min` just the ocaml toplevel
* `full` includes camlp4, lwt, js_of_ocaml and their syntax extentions

It is very useful with the JavaScript kernels to also serve some part of the
filesystem.  For example:

```
$ iocaml -js min -serve /home/andyman/.opam 
```

This would allow files from the `.opam` directory to be read from the
toplevel.

* `-serve path` serves files from path to the same uri
* `-serve-at uri path` serves files from path at the given uri
* `-serve-jslibs` configure file server to serve system libraries for the JavaScript kernels.

Multiple `-serve` options can be specified and they are tested in order
until a file is found.

The `js_of_ocaml` psuedo file system has been setup so you can use the standard
file I/O facilities to access served files (read only at the moment, write
support is a future possibliity).
