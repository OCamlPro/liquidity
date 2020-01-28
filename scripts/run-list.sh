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

DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
. $DIR/config.sh

CMD=$1
shift

FAIL=0
TOTAL=0

for i in "$@"; do
    $CMD "$i" || FAIL=$((FAIL + 1)) ;
    TOTAL=$((TOTAL + 1));
done

if [[ "$FAIL" -eq 0 ]] ; then
    echo -e "\n${GREEN}${BOLD}All $TOTAL tests passed for$DEFAULT$BOLD $CMD !$DEFAULT\n"
else
    echo -e "\n${RED}${BOLD}$FAIL failed out of $TOTAL tests for$DEFAULT$BOLD $CMD !$DEFAULT\n";
    exit 2
fi
