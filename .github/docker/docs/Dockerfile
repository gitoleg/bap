
FROM ocaml/opam2:ubuntu-19.04

WORKDIR /home/opam

RUN sudo apt-get --yes install emacs-nox man2html pkg-config \
 && opam switch 4.07 \
 && eval $(opam env) \
 && opam update \
 && git clone https://github.com/BinaryAnalysisPlatform/bap \
 && opam depext bap --yes \
 && opam pin add bap bap \
 && opam install odig --yes \
 && eval $(opam env)

#COPY intro.mld /home/opam/
#COPY update.sh /home/opam/

#ENTRYPOINT ["sh", "/home/opam/update.sh"]