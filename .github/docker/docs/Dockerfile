
FROM ocaml/opam2:ubuntu

WORKDIR /home/opam

RUN sudo apk add libxml2-dev emacs man \
 && opam switch 4.09 \
 && eval $(opam env) \
 && opam update \
 && git clone https://github.com/BinaryAnalysisPlatform/bap \
 && opam pin add bap bap \
 && opam depext --install bap --yes \
 && opam install odig --yes \
 && eval $(opam env)

#COPY intro.mld /home/opam/
#COPY update.sh /home/opam/

#ENTRYPOINT ["sh", "/home/opam/update.sh"]