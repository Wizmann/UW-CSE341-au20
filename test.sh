set -e

pushd () {
    command pushd "$@" > /dev/null
}

popd () {
    command popd "$@" > /dev/null
}

pushd hw0
ocaml hw0_test.ml
popd

pushd hw1
ocaml hw1_test.ml
popd

pushd hw2
ocaml hw2test.ml
ocaml hw2challenge_test.ml
popd

pushd hw3
ocaml hw3test.ml
popd
