#!/bin/bash

test=$1

DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
. $DIR/config.sh

LIQUIDITY=liquidity
LIQUID_FULL_PATH=./${LIQUIDITY}
LIQARGS="--verbose"
LIQEXEC="${LIQUID_FULL_PATH} ${LIQARGS}"

printf "${BOLD}%-20s${DEFAULT}" "$(basename $test)"

mkdir -p $(dirname "_obuild/tests/$test")

run \
    "Compile" \
    "${LIQEXEC} tests/$test.liq -o _obuild/tests/$test.tz"

run \
    "Typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

run \
    "Decompile" \
    "${LIQEXEC} _obuild/tests/$test.tz"

run \
    "Re-compile" \
    "${LIQEXEC} _obuild/tests/${test}.tz.liq"

run \
    "Re-typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

echo
