#!/bin/bash

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

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $DIR/env.sh

export OPAMYES=1

make clone-dune-network
pushd $DUNE
make build-deps || (rm -rf _opam; make build_deps)
eval `opam config env`
make
popd
