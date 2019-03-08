#!/bin/bash

test=$1

DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
. $DIR/config.sh

LIQUIDITY=liquidity

printf "${BOLD}%-20s${DEFAULT}" "$(basename $test)"

mkdir -p $(dirname "_obuild/tests/$test")

run \
    "Compile" \
    "./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/$test.liq -o _obuild/tests/$test.tz"

run \
    "Typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

run \
    "Decompile" \
    "./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm _obuild/tests/$test.tz"

run \
    "Re-compile" \
    "./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm _obuild/tests/$test.tz.liq"

run \
    "Re-typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

echo
