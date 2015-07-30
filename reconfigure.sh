#!/usr/bin/env bash
cabal2nix ./ > default.nix
cabal2nix --shell ./ > shell.nix
nix-shell -I ~ --command 'cabal configure --enable-tests'
