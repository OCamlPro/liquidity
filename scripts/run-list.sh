#!/bin/bash

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
