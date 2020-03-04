
FROM ocaml/opam2:alpine

WORKDIR /home/opam

RUN sudo apk add libxml2-dev emacs \
 && opam switch 4.09 \
 && eval $(opam env)
 && opam update \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
 && opam depext --install bap --yes \
 && opam install odig --yes \
 && eval $(opam env)

COPY update.sh /home/opam/

ENTRYPOINT ["sh", "/home/opam/update.sh"]