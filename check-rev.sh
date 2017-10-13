#!/bin/sh

echo '============================================================='
echo

DEFAULT='\033[0m'
RED='\033[0;31m'

TESTDIR=$1
test=$2

echo "\n[check-rev.sh] test = $test, TESTDIR = $TESTDIR"

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    echo "Testing $test.tz ---------------------------------------------"
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of $TESTDIR/$test.tz skipped${DEFAULT}\n"
fi

echo "Generating $test.tz.liq --------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz || exit 2

echo "Compiling $test.tz.liq ---------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz.liq || exit 2

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    echo "Testing $test.tz.liq.tz --------------------------------------"
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz.liq.tz || exit 2
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of $TESTDIR/$test.tz.liq.tz skipped${DEFAULT}\n"
fi

echo
echo GOOD
echo

