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

echo ${LIQEXEC} tests/$test.liq
${LIQEXEC} tests/$test.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script tests/$test.liq.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of tests/$test.liq.tz skipped${DEFAULT}\n"
fi

echo ${LIQEXEC} tests/$test.liq.tz
${LIQEXEC} tests/$test.liq.tz || exit 2

echo ${LIQEXEC} tests/${test}_liq.tz.liq
${LIQEXEC} tests/${test}_liq.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script tests/${test}_liq_tz.liq.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of tests/${test}_liq_tz.liq.tz skipped${DEFAULT}\n"
fi
