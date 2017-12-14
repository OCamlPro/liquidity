#!/bin/sh

#TEZOS_CLIENT=./_build/default/src/client_main.exe
#TEZOS_NODE=./_build/default/src/node_main.exe
TEZOS_CLIENT=./_obuild/tezos-client/tezos-client.asm
TEZOS_NODE=./_obuild/tezos-node/tezos-node.asm

rm -rf /tmp/tezos

nohup $TEZOS_NODE  run --sandbox test/sandbox.json --data-dir "/tmp/tezos" --rpc-addr 127.0.0.1:8732 &

sleep 3

# Now, the server is running the Genesis protocol, so we need to switch
# to the Alpha protocol to run Michelson programs...

rm -rf ~/.tezos-client

$TEZOS_CLIENT -block genesis \
   activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
   with fitness 4 \
   and passes 3 \
   and key edskRhxswacLW6jF6ULavDdzwqnKJVS4UcDTNiCyiH6H8ZNnn2pmNviL7pRNz9kRxxaWQFzEQEcZExGHKbwmuaAcoMegj5T99z

$TEZOS_CLIENT -block head \
    add public key bootstrap1 \
    edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav

$TEZOS_CLIENT -block head \
    add secret key bootstrap1 \
    edskRuR1azSfboG86YPTyxrQgosh5zChf5bVDmptqLTb5EuXAm9rsnDYfTKhq7rDQujdn5WWzwUMeV3agaZ6J2vPQT58jJAJPi

