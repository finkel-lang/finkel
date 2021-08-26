{
  nixpkgs ? <nixpkgs>,
  compiler ? "ghc8106"
}:

let
  pkgs = import ./nix/finkel-packages.nix {
    inherit compiler nixpkgs;
  };
in pkgs.finkelPackages
