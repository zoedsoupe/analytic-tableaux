{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  name = "analytic_tableaux";

  buildInputs = with haskellPackages; [
    ghc
    stack
    hlint # style linter
    fourmolu # formatter
    HUnit # unit test framework
    QuickCheck # property based test framework
  ];
}
