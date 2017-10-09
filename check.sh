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

echo ./_obuild/ocp-liquidity-comp/ocp-liquidity-comp.asm -I +../zarith zarith.cma -I _obuild/liquidity-env ./_obuild/liquidity-env/liquidity-env.cma -impl tests/$test.liq
./_obuild/ocp-liquidity-comp/ocp-liquidity-comp.asm -I +../zarith zarith.cma -I _obuild/liquidity-env ./_obuild/liquidity-env/liquidity-env.cma -dsource -impl tests/$test.liq
