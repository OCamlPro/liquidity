# This script is used in .travis.yml for continuous integration on travis.
# BTW, it also show some needed system packages to build liquidity
# Travis CI is done on Ubuntu trusty

wget -qq https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
export OPAMYES=1

# currently, we only target OCaml 4.05.0 because we reuse parser of OCaml 4.05.0
opam init --comp 4.05.0

eval `opam config env`

opam update
opam install ocp-build zarith uutf uri uchar stringext sexplib re ocplib-endian jsonm hex ezjsonm cstruct calendar ocurl nocrypto sodium
opam pin add ocplib-json-typed --dev
# TODO > other deps are missing ?
