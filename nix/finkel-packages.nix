# Main configuration settings for finkel packages

{
  nixpkgs,
  compiler
} :

let

  # Separating the "cabal2nix" from host nixpkgs to target nixpkgs with using
  # "pkgs.haskellPackages.haskellSrc2nix" and
  # "pkgs.haskellPackages.callPackage". The resulting ".nix" file generated from
  # cabal2nix is a plain nix script taking "mkDerivation" from its argument.

  hostNixPkgs = import <nixpkgs> {};

  haskellSrc2nix = hostNixPkgs.haskellPackages.haskellSrc2nix;

  filtPattern =
    pkgs.nix-gitignore.gitignoreFilterPure (_:_: true) ../.gitignore ../.;

  targets =
    pkgs.lib.mapAttrs (name: src0:
      let
        src = builtins.filterSource filtPattern src0;
      in haskellSrc2nix {
        inherit name src;
      }
    ) {
      finkel-kernel = ../finkel-kernel;
      fkc = ../fkc;
      finkel-setup = ../finkel-setup;
      finkel-core = ../finkel-core;
      finkel-tool= ../finkel-tool;
      finkel = ../finkel;
    };

  # To compile finkel-core and finkel-tool with ghc 8.10.3. At the moment,
  # running fkc with multiple cores not working well.
  moreCabalOptions =
    if compiler == "ghc8103" then
      { maxBuildCores = 1; }
    else
      { };

  # Main configuration for finkel related packages.
  overlay = self: super: {
    haskellPackages = super.haskell.packages.${compiler}.override {
      overrides = hself: hsuper:
        (
          builtins.mapAttrs (name: drv:
            let
              reified = hsuper.callPackage drv {};
            in
              super.haskell.lib.overrideCabal reified (old: {
                # To descrease the size of executables, as done in ghc.
                enableSharedExecutables = true;
              }) // moreCabalOptions
          ) targets
        ) // {
          doc =
            # The 'doc' package contains test codes only, overwriting the
            # installPhase to skip the works done for library and executable
            # packages.
            let
              drv = haskellSrc2nix {
                name = "doc";
                src = ../doc;
              };
              reified  = hsuper.callPackage drv {};
            in
              super.haskell.lib.overrideCabal reified (old:
                {
                  doHaddock = false;
                  installPhase = ''
runHook preInstallPhase
mkdir -p $out
runHook postInstallPhase
'';
                }
              );
        };
    };
  };

  pkgs = import "${nixpkgs}" {
    overlays = [overlay];
  };

  finkelPackages = with pkgs.haskellPackages; {
    inherit finkel-kernel fkc finkel-setup finkel-core finkel-tool finkel doc;
  };

in pkgs // { inherit finkelPackages; }
