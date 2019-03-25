#!/bin/bash

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
