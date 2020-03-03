#!/usr/bin/env sh

if [ "path$1" != "path" ]; then
    echo "copying documentation ... "
    cp -r `opam config var prefix`/var/cache/odig/html $1/
    cp lisp.html $1/
fi
