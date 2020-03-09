
FROM ocaml/opam2:ubuntu-19.04

WORKDIR /home/opam

RUN sudo apt-get --yes install emacs-nox man2html pkg-config \
 && opam switch 4.07 \
 && eval $(opam env) \
 && opam update \
 && opam repo add bap git://github.com/gitoleg/opam-repository#testing \
 && opam depext bap --yes --install \
 && opam install odig --yes \

RUN
 git clone https://github.com/BinaryAnalysisPlatform/bap \
 && cd bap \
 && make doc
 && ls doc

#COPY intro.mld /home/opam/
#COPY update.sh /home/opam/

#ENTRYPOINT ["sh", "/home/opam/update.sh"]