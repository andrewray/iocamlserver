# install OCaml + OPAM
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

OPAM=opam-1.2.0-beta4-i686-Linux

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
wget https://github.com/ocaml/opam/releases/download/1.2.0-beta4/$OPAM
chmod +x $OPAM
export OPAMYES=1
ls -la
pwd
file $OPAM
./$OPAM init 
eval `opam config env`
./$OPAM remote add iocaml-dev git://github.com/andrewray/opam.git
./$OPAM update
./$OPAM install iocaml.999.9.9 

