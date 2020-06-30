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
LIQUID_FULL_PATH=./${LIQUIDITY}
# Using json output as Love pretty-printer is incorrect
LIQARGS="--verbose --no-ignore-annots --target Love --json"
LIQEXEC="${LIQUID_FULL_PATH} ${LIQARGS}"

TESTNAME=$(basename $test)
printf "${BOLD}%-20s${DEFAULT}" "${TESTNAME%.*}"

mkdir -p $(dirname "_obuild/tests/$test")

run \
    "Compile" \
    "echo '{\"program\":' > _obuild/tests/$test.lov.json &&
     ${LIQEXEC} tests/$test -o - >> _obuild/tests/$test.lov.json &&
     echo '}' >> _obuild/tests/$test.lov.json "

run \
    "Typecheck" \
    "${DUNE_FULL_PATH} ${DUNE_ARGS} rpc post /chains/main/blocks/head/helpers/scripts/typecheck_code with file:_obuild/tests/$test.lov.json" \
    $([ -f ${DUNE_FULL_PATH} ] ; echo $?)

echo
