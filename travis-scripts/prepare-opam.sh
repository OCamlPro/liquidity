# This script is used in .travis.yml for continuous integration on travis.
# BTW, it also show some needed system packages to build liquidity
# Travis CI is done on Ubuntu trusty

wget -qq https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
export OPAMYES=1

# currently, we only target OCaml 4.05.0 because we reuse parser of OCaml 4.05.0
opam init --comp 4.05.0

eval `opam config env`

opam update
opam install ocp-build zarith ocplib-json-typed ocplib-endian calendar

# TODO > other deps are missing ?

opam install astring base64 biniou camlp4 cmdliner cohttp cohttp-lwt cohttp-lwt-unix conduit conduit-lwt conduit-lwt-unix conf-autoconf conf-which cstruct depext easy-format ezjsonm fieldslib fmt hex ipaddr js_of_ocaml js_of_ocaml-camlp4 js_of_ocaml-compiler js_of_ocaml-lwt js_of_ocaml-ppx js_of_ocaml-toplevel js_of_ocaml-tyxml jsonm logs lwt magic-mime markup menhir ocp-indent ocp-ocamlres omd optcomp pprint ppx_cstruct ppx_fields_conv  ppx_tools ppx_tools_versioned  react reactiveData topkg tyxml uchar uutf yojson

