#!/bin/sh

test=$1
echo $test

TESTDIR=tests/reverse

./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz || exit 2
./_obuild/liquidity/liquidity.asm $TESTDIR/$test.tz.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program $TESTDIR/$test.tz.liq.tz

