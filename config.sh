#!/bin/sh

export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y

TEZOS_FULL_PATH=$(which tezos-client || echo "./tezos/tezos-client")
# TEZOS_ARGS="--addr next.tzscan.io --port 18732"
