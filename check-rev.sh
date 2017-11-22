#!/bin/sh

echo '============================================================='
echo

DEFAULT='\033[0m'
RED='\033[0;31m'

TESTDIR=$1
test=$2

. ./config.sh

echo "\n[check-rev.sh] test = $test, TESTDIR = $TESTDIR"

if [ -f ${TEZOS_FULL_PATH} ] ; then
    echo "Testing $test.tz ---------------------------------------------"
    ${TEZOS_FULL_PATH} typecheck program $TESTDIR/$test.tz
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of $TESTDIR/$test.tz skipped${DEFAULT}\n"
fi

echo "Generating $test.tz.liq --------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz || exit 2

echo "Compiling $test.tz.liq ---------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz.liq || exit 2

if [ -f ${TEZOS_FULL_PATH} ] ; then
    echo "Testing $test.tz.liq.tz --------------------------------------"
    ${TEZOS_FULL_PATH} typecheck program $TESTDIR/$test.tz.liq.tz || exit 2
else
    echo "\n${RED}${TEZOS_FULL_PATH} not present ! typechecking of $TESTDIR/$test.tz.liq.tz skipped${DEFAULT}\n"
fi

echo
echo GOOD
echo

