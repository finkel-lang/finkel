{
  nixpkgs ? <nixpkgs>,
  compiler ? "ghc8104"
}:

let
  pkgs = import ./nix/finkel-packages.nix {
    inherit compiler nixpkgs;
  };
in pkgs.finkelPackages
