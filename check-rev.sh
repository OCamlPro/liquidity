#!/bin/sh

echo '============================================================='
echo

DEFAULT='\033[0m'
RED='\033[0;31m'

TESTDIR=$1
test=$2

. ./config.sh

mkdir -p $(dirname "_obuild/tests/$test")

echo "\n[check-rev.sh] test = $test, TESTDIR = $TESTDIR"

if [ -f ${TEZOS_FULL_PATH} ] ; then
    echo "Testing $test.tz ---------------------------------------------"
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script $TESTDIR/$test.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of $TESTDIR/$test.tz skipped${DEFAULT}\n"
fi

echo "Generating _obuild/tests/$test.tz.liq --------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz -o _obuild/tests/$test.tz.liq || exit 2

echo "Compiling _obuild/tests/$test.tz.liq ---------------------------------------"
./_obuild/liquidity/liquidity.asm _obuild/tests/$test.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    echo "Testing _obuild/tests/${test}_tz.tz --------------------------------------"
    ${TEZOS_FULL_PATH} ${TEZOS_ARGS} typecheck script _obuild/tests/${test}_tz.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of _obuild/tests/${test}_tz.tz skipped${DEFAULT}\n"
fi

echo
echo GOOD
echo

