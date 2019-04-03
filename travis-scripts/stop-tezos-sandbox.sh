#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $DIR/env.sh

if [ -e "$TEZOS_PID_FILE" ]
then
    echo "Killing Tezos node..."
    kill `cat $TEZOS_PID_FILE`
    rm -f $TEZOS_PID_FILE
else
    echo "Tezos node was not started."
fi
