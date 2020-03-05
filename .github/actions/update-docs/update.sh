#!/usr/bin/env sh

eval $(opam env)

echo "building lisp documentation"
bap /bin/true --primus-lisp-documentation > lisp.org \
emacs lisp.org --batch --eval '(org-html-export-to-html)'

echo "building odoc documentaion"
odig odoc --index-title='Binary Analysis Platform' \
     --index-intro=intro.mld --no-tag-index bap bap-api bap-abi bap-arm bap-beagle-prey bap-bml bap-bundle bap-byteweight bap-c bap-demangle bap-dwarf bap-elementary bap-elf bap-ida bap-llvm bap-main bap-plugins bap-primus bap-recipe bap-strings bap-taint bap-traces bap-x86-cpu bitvec-binprot bitvec-order bitvec-sexp graphlib monads ogre regular text-tags

# removing symbolic links
for f in `find $(odig cache path) -type l`; do
    rm $f
done

# adding documentaion
git clone https://${repo} bap.io
cd bap.io
mv ../lisp.html bap/api/lisp/index.html
git add bap/api/lisp/index.html

cp -r $(odig cache path)/html/* bap/api/odoc/
git add bap/api/odoc

# setup and push
repo="github.com/gitoleg/gitoleg.github.io"
remote_repo="https://${GITHUB_ACTOR}:${INPUT_SECRET}@${repo}.git"

git config --global user.name ${GITHUB_ACTOR}
git config --global user.email ${GITHUB_ACTOR}@users.noreply.github.com
git remote set-url origin $remote_repo

msg=`bap --version`
git commit -m "$msg"

git push origin master