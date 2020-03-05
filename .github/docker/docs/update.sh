#!/usr/bin/env sh

set -eu

eval $(opam env)

fake_ida="/home/opam/fake-ida-7.1"
mkdir $fake_ida
cd $fake_ida
for f in "ida ida64 idat idat64"; do
    touch $f
    chmod +x $f
done
mkdir plugins
cd ../
opam install bap-ida
cp /home/opam/.opam/4.09/share/bap-ida-python/plugin_loader_bap.py /home/opam/fake-ida-7.1/plugins/

echo "building lisp documentation"
bap /bin/true --primus-lisp-documentation > lisp.org
emacs lisp.org --batch --eval '(org-html-export-to-html)'

echo "building odoc documentaion"
odig odoc --index-title='Binary Analysis Platform' \
     --index-intro=/home/opam/intro.mld --no-tag-index bap bap-api bap-abi bap-arm bap-beagle-prey bap-bml bap-bundle bap-byteweight bap-c bap-demangle bap-dwarf bap-elementary bap-elf bap-ida bap-llvm bap-main bap-plugins bap-primus bap-recipe bap-strings bap-taint bap-traces bap-x86-cpu bitvec-binprot bitvec-order bitvec-sexp graphlib monads ogre regular text-tags

# removing symbolic links
for f in `find $(odig cache path) -type l`; do
    rm $f
done

repo="github.com/gitoleg/gitoleg.github.io"

# adding documentaion
# git clone https://${repo} bap.io
# cd bap.io
# mv ../lisp.html bap/api/lisp/index.html
# git add bap/api/lisp/index.html
# git rm -r bap/api/odoc/
# mkdir bap/api/odoc/
# cp -r $(odig cache path)/html/* bap/api/odoc/
# git add bap/api/odoc

## setup and push
# remote_repo="https://${1}:${2}@${repo}.git"

# git config --global user.name ${1}
# git config --global user.email ${1}@users.noreply.github.com
# git remote set-url origin $remote_repo

# msg=`bap --version`
# git commit -m "$msg"

#git push origin master
