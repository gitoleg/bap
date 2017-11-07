#!/bin/sh

# TODO add a BAP/bap-veri repository BinaryAnalysisPlatform
if [ "$BAP_RUN_VERI" = true ]; then
    git clone https://github.com/gitoleg/bap-veri.git
    opam pin add bap-veri bap-veri/ -n
    opam install -v bap-veri
    make veri
fi
