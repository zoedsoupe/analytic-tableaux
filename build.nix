{ pkgs ? import <nixpkgs> {} }:

with pkgs;

haskellPackages.callPackage ./analytic-tableaux.nix {}
