#!/bin/sh

DEFAULT='\033[0m'
RED='\033[0;31m'

test=$1
echo "\n[check-mini.sh] test = $test"

. ./config.sh

LIQUIDITY=liquidity

mkdir -p $(dirname "_obuild/tests/$test")

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/$test.liq -o _obuild/tests/$test.tz || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/$test.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of _obuild/tests/$test.tz skipped${DEFAULT}\n"
fi

./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm _obuild/tests/$test.tz || exit 2

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm _obuild/tests/$test.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of _obuild/tests/${test}_tz.tz skipped${DEFAULT}\n"
fi
