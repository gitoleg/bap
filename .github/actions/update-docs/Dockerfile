
FROM ocaml/opam2:alpine

WORKDIR /home/opam

RUN sudo apk add libxml2-dev emacs \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes \
 && opam install odig --yes \
 && eval $(opam env)

COPY update.sh /home/opam/

ENTRYPOINT ["sh", "/home/opam/update.sh"]