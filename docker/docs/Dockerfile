FROM ocaml/opam2:alpine

WORKDIR /home/opam

RUN sudo apk add libxml2-dev emacs \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes \
 && opam install odig --yes \
 && eval $(opam env) \
 && odig odoc \
 && bap /bin/true --primus-lisp-documentation > lisp.org \
 && emacs lisp.org --batch --eval '(org-html-export-to-html)'

COPY entrypoint.sh /home/opam/

ENTRYPOINT ["sh", "/home/opam/entrypoint.sh"]
