#!/bin/sh

DEFAULT='\033[0m'
RED='\033[0;31m'

test=$1
echo "\n[check.sh] test = $test"

. ./config.sh

LIQUIDITY=liquidity
LIQUID_FULL_PATH=./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm
LIQARGS=--verbose
LIQEXEC="${LIQUID_FULL_PATH} ${LIQARGS}"

mkdir -p $(dirname "_obuild/tests/$test")

echo ${LIQEXEC} tests/$test.liq -o _obuild/tests/$test.tz
${LIQEXEC} tests/$test.liq -o _obuild/tests/$test.tz || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of _obuild/tests/$test.tz skipped${DEFAULT}\n"
fi

echo ${LIQEXEC} _obuild/tests/$test.tz
${LIQEXEC} _obuild/tests/$test.tz || exit 2

echo ${LIQEXEC} _obuild/tests/${test}.tz.liq
${LIQEXEC} _obuild/tests/${test}.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of _obuild/tests/${test}_tz.tz skipped${DEFAULT}\n"
fi
