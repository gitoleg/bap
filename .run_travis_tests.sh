TASK=$1

#eval `opam config env`

comp=4.05.0

mkdir -p $HOME/.opam/$comp/

ls -l $HOME
ls -l $HOME/save_opam/
ls -l $HOME/save_opam/bin/*

cp -r $HOME/save_opam/bin/ $HOME/.opam/$comp/
cp -r $HOME/save_opam/share/ $HOME/.opam/$comp/
cp -r $HOME/save_opam/lib/ $HOME/.opam/$comp/

export PATH=$HOME/.opam/$comp/bin:$PATH


ls -l ~/.opam/$comp/lib/bap

bap --version


if [ "$TASK" == "checks" ]; then
    bash -exc 'make check'
fi

if [ "$TASK" == "unit_tests" ]; then
    bap_run_tests
fi

if [ "$TASK" == "veri" ]; then
    bap-veri --help
    bash -exc 'make veri'
fi
