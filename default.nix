{
  nixpkgs ? <nixpkgs>,
  compiler ? "ghc8103"
}:

let
  pkgs = import ./nix/finkel-packages.nix {
    inherit compiler nixpkgs;
  };
in pkgs.finkelPackages
