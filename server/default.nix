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

  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  {
    icepeak-server = pkgs.haskellPackages.icepeak;
  }
