#!/bin/bash

test=$1

DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
. $DIR/config.sh

LIQARGS="--verbose --no-ignore-annots"

LIQUIDITY=liquidity
LIQUID_FULL_PATH=./${LIQUIDITY}
LIQEXEC="${LIQUID_FULL_PATH} ${LIQARGS}"

LIQUIDITY_MINI=liquidity-mini
LIQUID_MINI_FULL_PATH=./${LIQUIDITY_MINI}
LIQMINI="${LIQUID_MINI_FULL_PATH} ${LIQARGS}"

TESTNAME=$(basename $test)
printf "${BOLD}%-20s${DEFAULT}" "${TESTNAME%.*}"

mkdir -p $(dirname "_obuild/tests/$test")

run \
    "Compile" \
    "${LIQMINI} tests/$test -o _obuild/tests/$test.tz"

run \
    "Typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] ; echo $?)

run \
    "Decompile" \
    "${LIQEXEC} _obuild/tests/$test.tz -o _obuild/tests/$test.tz.liq" \
    $([ -f ${LIQUID_FULL_PATH} ] ; echo $?)

run \
    "Re-compile" \
    "${LIQMINI} _obuild/tests/$test.tz.liq -o _obuild/tests/${test}_tz.tz" \
    $([ -f ${LIQUID_FULL_PATH} ] ; echo $?)

run \
    "Re-typecheck" \
    "${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz" \
    $([ -f ${TEZOS_FULL_PATH} ] && [ -f ${LIQUID_FULL_PATH} ] ; echo $?)

echo
