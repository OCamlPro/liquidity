#!/bin/sh

test=$1
echo $test

./_obuild/liquidity/liquidity.asm tests/$test.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz


./_obuild/liquidity/liquidity.asm tests/$test.liq.tz || exit 2

./_obuild/liquidity/liquidity.asm tests/$test.liq.tz.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz.liq.tz
