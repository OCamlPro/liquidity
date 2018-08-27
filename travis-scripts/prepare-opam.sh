# This script is used in .travis.yml for continuous integration on travis.
# BTW, it also show some needed system packages to build liquidity
# Travis CI is done on Ubuntu trusty

export OPAMYES=1
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
yes "" | sh install.sh

# currently, we only target OCaml 4.06.1 because we reuse the parser of OCaml
opam switch create liquidity 4.06.1

eval $(opam config env)

opam update
opam install ocp-build ocplib-endian zarith calendar digestif hex ocurl lwt lwt_log uri sodium bigstring ezjsonm
make clone-tezos
tezos/scripts/install_build_deps.raw.sh
# make -C tezos build-deps
