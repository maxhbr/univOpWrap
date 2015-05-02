#!/usr/bin/env bash
TMP="$(mktemp)"
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
pushd $DIR
[[ -f ./.hdevtools.sock ]] && mv ./.hdevtools.sock $TMP
nix-shell --command "cabal build" && cp ./dist/build/univOpWrap/univOpWrap ~/bin
[[ -f $TMP ]] && mv $TMP ./.hdevtools.sock
popd
