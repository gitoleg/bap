#!/usr/bin/env sh

# setup and push
repo="github.com/gitoleg/gitoleg.github.io"
remote_repo="https://${GITHUB_ACTOR}:${INPUT_SECRET}@${repo}.git"

# adding documentaion
git clone https://${repo} bap.io
cd bap.io
mv ../lisp.html bap/api/lisp/index.html
git add bap/api/lisp/index.html

cp -Lr $(odig cache path)/html/* bap/api/odoc/
cp lisp/lisp.html
git add bap/api/odoc

git config --global user.name ${GITHUB_ACTOR}
git config --global user.email ${GITHUB_ACTOR}@users.noreply.github.com
git remote set-url origin $remote_repo

msg=`bap --version`
git commit -m "$msg"

git push origin master
