############################################################################
#                               Liquidity                                  #
#                                                                          #
#                  Copyright (C) 2017-2019 OCamlPro SAS                    #
#                                                                          #
#                    Authors: Fabrice Le Fessant                           #
#                             Alain Mebsout                                #
#                             David Declerck                               #
#                                                                          #
#  This program is free software: you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation, either version 3 of the License, or       #
#  (at your option) any later version.                                     #
#                                                                          #
#  This program is distributed in the hope that it will be useful,         #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of          #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
#  GNU General Public License for more details.                            #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.  #
############################################################################

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
