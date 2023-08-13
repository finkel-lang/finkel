{
  nixpkgs ? <nixpkgs>,
  compiler ? "ghc8104",
  tag ? "latest",
  created ? "now",
  stream ? true
}:

let
  myPkgs = import ./finkel-packages.nix {
    inherit nixpkgs compiler;
  };
  myHaskellPackages = myPkgs.haskellPackages;
  myGhc = myHaskellPackages.ghcWithPackages (p: [
    p.finkel-kernel
    p.finkel-setup
    p.finkel-core
    p.finkel-tool
  ]);
  buildImage =
    if stream then
      myPkgs.dockerTools.streamLayeredImage
    else
      myPkgs.dockerTools.buildLayeredImage;

in buildImage {
  name = "finkel";
  tag = "${tag}";
  created = "${created}";
  contents = [
    myPkgs.busybox
    myHaskellPackages.cabal-install
    # Compilation of stack is failing, commented out for now.
    # myPkgs.stack
    myGhc
    myHaskellPackages.fkc
    myHaskellPackages.fnkpp
    myHaskellPackages.finkel
  ];
  config = {
    # Cmd = [ "/bin/finkel" "repl" "-B${myGhc}/lib/ghc-${myGhc.version}" ];
    Cmd = [ "/bin/sh" ];
    Volumes = {
      "/tmp" = { };
    };
  };
}
