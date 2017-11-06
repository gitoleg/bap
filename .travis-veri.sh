#!/bin/sh

git clone https://github.com/BinaryAnalysisPlatform/bap-veri.git
opam install core.v0.9.1 pcre textutils
cd bap-veri
oasis setup
./configure --prefix=`opam config var prefix`
make
make install
cd ../
cd bap
ls
make veri
