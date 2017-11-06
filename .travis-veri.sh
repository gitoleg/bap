#!/bin/sh

git clone https://github.com/BinaryAnalysisPlatform/bap-veri.git
opam install pcre textutils
cd bap-veri
oasis setup
./configure --prefix=`opam config var prefix`
make
make install
