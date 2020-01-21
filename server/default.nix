{ # We expect getPkgs to be a lambda that gives us a package set. Example:
  # the lambda in `nixpkgs/default.nix` without any arguments passed to it.
  # That allows us to pass in config later, while allowing users to pass in
  # their own version of Nixpkgs.
  getPkgs ? import ../nix/nixpkgs.nix
}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          icepeak = haskellPackagesNew.callPackage ./server.nix { };
          websockets = haskellPackagesNew.callPackage ../nix/websockets.nix { };
        };
      };
    };
  };

  pkgs = getPkgs { inherit config; };
in
  {
    icepeak-server = pkgs.haskellPackages.icepeak;
  }
