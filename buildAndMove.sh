#!/usr/bin/env bash
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
pushd $DIR
nix-shell --command "cabal build" && cp ./dist/build/univOpWrap/univOpWrap ~/bin
popd
