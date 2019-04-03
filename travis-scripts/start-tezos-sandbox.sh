#!/bin/bash
shopt -s expand_aliases

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $DIR/env.sh

rm -f $TEZOS_PID_FILE
eval `opam config env`

$TEZOS/src/bin_node/tezos-sandboxed-node.sh 5 &> /tmp/tezos-log &
echo $! > $TEZOS_PID_FILE

echo "Waiting a bit for node to be started"
sleep 5
eval `$TEZOS/src/bin_client/tezos-init-sandboxed-client.sh 5`

tezos-activate-alpha
