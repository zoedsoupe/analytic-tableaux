# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "An analytix tableaux prover and solver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        packageName = "analytic-tableaux";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            ghcid
            stack
            hlint # style linter
            fourmolu # formatter
            hspec # testing framework
            hspec-parsec # expectations for testing parsers
            HUnit # unit test framework
            QuickCheck # property based test framework
            quickcheck-text # gen utf8 chars
            prettyprinter # modern prettyprinter
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
