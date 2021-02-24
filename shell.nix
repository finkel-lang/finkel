{
  nixpkgs ? <nixpkgs>,
  compiler ? "ghc8104",
  withHoogle ? false
}:

let
  hostPkgs = import <nixpkgs> {};

  pkgs = import ./nix/finkel-packages.nix {
    inherit compiler nixpkgs;
  };

  shell = pkgs.haskellPackages.shellFor {
    packages = _ :
      builtins.attrValues pkgs.finkelPackages;
    withHoogle = withHoogle;
    buildInputs= [
      hostPkgs.cabal-install
      hostPkgs.wget
    ];
  };
in shell
