#!/bin/sh

rm acceptedSeeds rejectedSeeds
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [hip])" cabal-install --run "cabal exec MapFinding -- -n 3000000"
