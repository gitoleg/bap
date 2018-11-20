#!/bin/bash

full_apt_version () {
  package=$1
  version=$2
  case "${version}" in
      latest) echo -n "${package}" ;;
      *) echo -n "${package}="
         apt-cache show "$package" \
             | sed -n "s/^Version: \(${version}\)/\1/p" \
             | head -1
  esac
}

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

upgrade_opam_file() {
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

    case "$OPAM_VERSION" in
        1.2)
            sudo apt-get install opam
            opam init --auto-setup --comp=$OCAML_VERSION --yes
            eval `opam config env` ;;
        2.0)
            install_bubblewrap
            bwrap --version
            echo "" | sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
            which opam
            opam --version

            echo "installing $OCAML_VERSION"

            opam init -v -v --compiler=$OCAML_VERSION --yes
            cat ~/.bash_profile

            eval $(opam env)
            which ocaml
            ls -la /home/travis/.opam/
            ;;
        *)
            echo "Unknown opam version $OPAM_VERSION"
            exit 1
            ;;
    esac
}

aptget_stuff
install_opam

echo $PATH

opam --version
ocaml -version
ls -la
opam install depext --yes

if [ "$OPAM_VERSION" == "2.0" ]; then
    opam install opam-state --yes
    upgrade_opam_file
fi

opam depext -y conf-m4
opam pin add travis-opam https://github.com/ocaml/ocaml-ci-scripts.git#master
cp ~/.opam/$(opam switch show)/bin/ci-opam ~/
opam remove -a travis-opam
mv ~/ci-opam ~/.opam/$(opam switch show)/bin/ci-opam

echo -en "travis_fold:end:prepare.ci\r"

opam config exec -- ci-opam
