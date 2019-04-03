#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
. $DIR/env.sh

export OPAMYES=1

pushd $TEZOS
make build-deps
eval `opam config env`
make
popd
