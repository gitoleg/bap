
FROM ocaml/opam2:ubuntu-19.04

WORKDIR /home/opam

RUN sudo apt-get --yes install emacs-nox man2html pkg-config llvm-dev \
 && opam switch 4.09 \
 && eval $(opam env) \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testingopam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext bap --yes \
 && opam remote remove bap \
 && opam depext ocurl --yes \
 && opam install odig conf-bap-llvm

RUN eval $(opam env) \
 && git clone https://github.com/BinaryAnalysisPlatform/bap \
 && opam pin add bap bapopam pin add bap bap \
 && cd bap \
 && make doc \
 && ls doc

#ENTRYPOINT ["sh", "/home/opam/update.sh"]
