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

shopt -s expand_aliases

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $DIR/env.sh

rm -f $TEZOS_PID_FILE
eval `opam config env`

$TEZOS/src/bin_node/tezos-sandboxed-node.sh 5 &> /tmp/tezos-log &
echo $! > $TEZOS_PID_FILE

echo "Waiting a bit for node to be started"
sleep 5
eval `$TEZOS/src/bin_client/tezos-init-sandboxed-client.sh 5`

tezos-activate-alpha
