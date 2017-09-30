#!/bin/sh

test=$1
echo $test

LIQUIDITY=liquidity

echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz


echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz || exit 2

echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz.liq 
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz.liq || exit 2

./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz.liq.tz
