{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShellNoCC { inputsFrom = [ (pkgs.haskellPackages.callPackage ./neither-data.nix) ]; }
