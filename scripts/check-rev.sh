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

test=$1

DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
. $DIR/config.sh

LIQUIDITY=liquidity

TESTNAME=$(basename $test)
printf "${BOLD}%-20s${DEFAULT}" "${TESTNAME%.*}"

mkdir -p $(dirname "_obuild/tests/$test")

run \
    "Typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script $test" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

run \
    "Decompile" \
    "./liquidity $test -o _obuild/tests/$test.liq"

run \
    "Re-compile" \
    "./liquidity _obuild/tests/$test.liq -o _obuild/tests/$test.liq.tz"

run \
    "Re-typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.liq.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

echo
