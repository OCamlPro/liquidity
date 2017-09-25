#!/bin/sh

#LIQUIDITY=liquidity
LIQUIDITY=liquidity-mini

test=$1
echo $test

./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz


./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz || exit 2

./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz.liq.tz
