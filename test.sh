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

pushd hw4
ocaml hw4test.ml
popd

pushd hw5
racket hw5tests.rkt
popd

pushd hw6
racket -l errortrace -t hw6tests.rkt 1> /dev/null
popd

pushd hw7
racket hw7tests.rkt
popd
