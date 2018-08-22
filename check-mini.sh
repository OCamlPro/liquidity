#!/bin/sh

DEFAULT='\033[0m'
RED='\033[0;31m'

test=$1
echo "\n[check-mini.sh] test = $test"

. ./config.sh

LIQUIDITY=liquidity

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/$test.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script tests/$test.liq.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of tests/$test.liq.tz skipped${DEFAULT}\n"
fi

./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz || exit 2

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/${test}_liq.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script tests/${test}_liq_tz.liq.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of tests/${test}_liq_tz.liq.tz skipped${DEFAULT}\n"
fi
