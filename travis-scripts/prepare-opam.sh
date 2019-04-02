# This script is used in .travis.yml for continuous integration on travis.
# Travis CI is done on Ubuntu trusty

export OPAMYES=1
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
yes "" | sh install.sh

# currently, we only target OCaml 4.06.1 because we reuse the parser of OCaml
opam switch create liquidity 4.06.1

eval $(opam config env)

opam update
make build-deps
make clone-tezos
