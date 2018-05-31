# This script is used in .travis.yml for continuous integration on travis.
# BTW, it also show some needed system packages to build liquidity
# Travis CI is done on Ubuntu trusty

wget -qq https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
export OPAMYES=1

# currently, we only target OCaml 4.06.1 because we reuse parser of OCaml
opam init --comp 4.06.1

eval `opam config env`

opam update
opam install ocp-build zarith uutf uri uchar stringext sexplib re lwt.3.3.0 ocplib-endian bigstring jsonm hex ezjsonm cstruct calendar ocurl digestif sodium
make clone-tezos
make -C tezos build-deps
opam install ocplib-json-typed ocplib-json-typed-bson
# TODO > other deps are missing ?
