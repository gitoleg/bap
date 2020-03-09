
FROM ocaml/opam2:ubuntu

WORKDIR /home/opam

RUN sudo apt-get --yes install emacs-nox man2html pkg-config \
 && opam switch 4.09 \
 && eval $(opam env) \
 && opam update \
 && git clone https://github.com/BinaryAnalysisPlatform/bap \
 && opam pin add bap bap -n \
 && opam depext --install bap \
 && opam install bap \
 && opam install odig --yes \
 && eval $(opam env)

#COPY intro.mld /home/opam/
#COPY update.sh /home/opam/

#ENTRYPOINT ["sh", "/home/opam/update.sh"]