#!/usr/bin/env
# Install a new system

SYSTEM=$1
shift

nix --extra-experimental-features "nix-command flakes" run \
    'github:nix-community/disko/latest#disko-install' -- \
    --flake '.#${SYSTEM}' $@
