#!/bin/sh

DEFAULT='\033[0m'
RED='\033[0;31m'

test=$1
echo "\n[check.sh] test = $test"

LIQUIDITY=liquidity

echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq || exit 2

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz || exit 2
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of tests/$test.liq.tz skipped${DEFAULT}\n"
fi

echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz || exit 2

echo ./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz.liq 
./_obuild/${LIQUIDITY}/${LIQUIDITY}.asm tests/$test.liq.tz.liq || exit 2

if [ -f ./tezos/_obuild/tezos-client/tezos-client.asm ] ; then
    ./tezos/_obuild/tezos-client/tezos-client.asm typecheck program tests/$test.liq.tz.liq.tz || exit 2
else
    echo "\n${RED}./tezos/_obuild/tezos-client/tezos-client.asm not present ! typechecking of tests/$test.liq.tz.liq.tz skipped${DEFAULT}\n"
fi

echo ./_obuild/ocp-liquidity-comp/ocp-liquidity-comp.asm -I +../zarith zarith.cma -I _obuild/liquidity-env ./_obuild/liquidity-env/liquidity-env.cma -impl tests/$test.liq
./_obuild/ocp-liquidity-comp/ocp-liquidity-comp.asm -I +../zarith zarith.cma -I _obuild/liquidity-env unix.cma ./_obuild/liquidity-env/liquidity-env.cma -dsource -impl tests/$test.liq || exit 2
rm -f a.out
