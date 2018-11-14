#!/bin/bash

set -uex

aptget_stuff() {
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

upgrade_opam() {
    cd opam

    cat > "upgrade.ml" << EOF
let filename = OpamFilename.of_string "opam"
let opamfile = OpamFile.OPAM.read (OpamFile.make filename)
let opamfile = OpamFormatUpgrade.opam_file opamfile
let () = OpamFile.OPAM.write (OpamFile.make filename) opamfile

let fields = {|
synopsis: "Binary Analysis Platform"
description: "Binary Analysis Platform"
|}

let opam = open_out_gen [Open_wronly; Open_append] 0o666 "opam"
let () = output_string opam fields
let () = close_out opam
EOF

    ocamlbuild -pkg opam-state upgrade.native
    ./upgrade.native
    cd ..
}

export OPAMYES=1

install_opam() {
    ls -l

    case "$OPAM_VERSION" in
        1.2)
            sudo apt-get install opam
            opam init --auto-setup --comp=$OCAML_VERSION --yes
            eval `opam config env` ;;
        2.0)
            install_bubblewrap
            echo "" | sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
            yes | opam init --auto-setup --comp=$OCAML_VERSION --yes
            eval $(opam env) ;;
        *)
            echo "Unknown opam version $OPAM_VERSION" ;;
    esac
}

aptget_stuff
install_opam
opam --version
ocaml -version

opam install depext --yes

if [ "$OPAM_VERSION" == "2.0" ]; then
    opam install opam-state --yes
    upgrade_opam
fi

opam depext -y conf-m4
opam pin add travis-opam https://github.com/ocaml/ocaml-ci-scripts.git#master
cp ~/.opam/$(opam switch show)/bin/ci-opam ~/
opam remove -a travis-opam
mv ~/ci-opam ~/.opam/$(opam switch show)/bin/ci-opam

echo -en "travis_fold:end:prepare.ci\r"

opam config exec -- ci-opam
