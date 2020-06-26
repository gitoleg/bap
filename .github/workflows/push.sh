#!/usr/bin/env sh

set -eu

TOKEN=$1

cd bap
bap_commit=`git rev-parse --short HEAD`

#TODO
git clone https://github.com/gitoleg/binaryanalysisplatform.github.io --no-checkout --single-branch --branch=add-actions --depth=1 blog

git reset --

mkdir -p blog/bap/api

cp -r  doc/man1 blog/bap/api/
cp -r  doc/man3 blog/bap/api/
cp -r  doc/lisp blog/bap/api/
cp -rL doc/odoc blog/bap/api/

cd blog

#TODO
#repo="https://${GITHUB_ACTOR}:${TOKEN}@github.com/${GITHUB_REPOSITORY}.git"
echo "actor is $GITHUB_ACTOR"
if [ "some$TOKEN" != "some" ]; then
    echo "has token"
fi

repo="https://gitoleg:${TOKEN}@github.com/${GITHUB_REPOSITORY}.git"

git config --global user.name $GITHUB_ACTOR
git config --global user.email "action-noreply@github.com"

git add bap/api

git commit -m $bap_commit
git push $repo master # TODO!
