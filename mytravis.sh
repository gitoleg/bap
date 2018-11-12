#!/bin/bash

set -uex

aptget() {
    sudo apt-get -y update
    sudo apt-get -y install \
         gcc make unzip libcap-dev m4 \
         git time curl clang aspcud libgmp-dev zlib1g-dev   \
         binutils-multiarch libcurl4-gnutls-dev
}

install_bubblewrap() {
    wget https://github.com/projectatomic/bubblewrap/releases/download/v0.3.1/bubblewrap-0.3.1.tar.xz
    tar xvf bubblewrap-0.3.1.tar.xz
    cd bubblewrap-0.3.1
    ./configure
    make
    sudo make install
    cd ..
}

install_opam() {
    case "$OPAM_VERSION" in
        1.2)
            sudo apt-get install opam
            opam init --auto-setup --comp=4.05.0 --yes
            eval `opam config env` ;;
        2.0)
            install_bubblewrap
            https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
            sudo ./install.sh
            opam init --auto-setup --comp=4.05.0 --yes
            eval $(opam env) ;;
        *)
            echo "Unknown opam version $OPAM_VERSION" ;;
    esac
}

aptget

install_opam
ocaml -version
opam --version
opam install depext --yes

opam depext -y conf-m4
opam pin add travis-opam https://github.com/ocaml/ocaml-ci-scripts.git#master
cp ~/.opam/$(opam switch show)/bin/ci-opam ~/
opam remove -a travis-opam
mv ~/ci-opam ~/.opam/$(opam switch show)/bin/ci-opam

echo -en "travis_fold:end:prepare.ci\r"
opam config exec -- ci-opam
