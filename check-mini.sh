#!/bin/sh

DEFAULT='\033[0m'
RED='\033[0;31m'

test=$1
echo "\n[check-mini.sh] test = $test"

LIQUIDITY=liquidity

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/$test.liq || exit 2

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of tests/$test.liq.tz skipped${DEFAULT}\n"
fi

./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz || exit 2

./_obuild/${LIQUIDITY}-mini/${LIQUIDITY}-mini.asm tests/$test.liq.tz.liq || exit 2

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz.liq.tz
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of tests/$test.liq.tz.liq.tz skipped${DEFAULT}\n"
fi
