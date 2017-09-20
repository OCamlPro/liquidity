#!/bin/sh

echo '============================================================='
echo

TESTDIR=$1
test=$2
echo $test

echo "Testing $test.tz ---------------------------------------------"
./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz 

echo "Generating $test.tz.liq --------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz || exit 2

echo "Compiling $test.tz.liq ---------------------------------------"
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz.liq || exit 2

echo "Testing $test.tz.liq.tz --------------------------------------"
./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz.liq.tz || exit 2

echo
echo GOOD
echo

