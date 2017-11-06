#!/bin/sh

git clone https://github.com/BinaryAnalysisPlatform/bap-veri.git
opam install bap-veri --deps-only -y
cd bap-veri
oasis setup
./configure --prefix=`opam config var prefix`
make
make install
