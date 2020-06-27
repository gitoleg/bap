#!/usr/bin/env sh

set -eu

TOKEN=$1
bap_commit=`git rev-parse --short HEAD`

#TODO
git clone https://github.com/gitoleg/binaryanalysisplatform.github.io --no-checkout --single-branch --branch=master --depth=1 blog

mkdir -p blog/bap/api

cd blog
git reset > /dev/null

echo "debug: status"
git status | head -n 10

echo "debug: copying"
cp -r  ../doc/man1 bap/api/
cp -r  ../doc/man3 bap/api/
cp -r  ../doc/lisp bap/api/
cp -rL ../doc/odoc bap/api/

echo "debug: status"
git status | head -n 10

repo="https://gitoleg:${TOKEN}@github.com/gitoleg/binaryanalysisplatform.github.io.git"

git config --global user.name $GITHUB_ACTOR
git config --global user.email "action-noreply@github.com"

echo "debug: git add"
git add bap/api
echo "debug: git commit"

git status | head -n 10

git commit -m $bap_commit  | head -n 10

echo "debug: pushing"
git push $repo master # TODO
